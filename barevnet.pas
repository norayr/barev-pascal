{
  Barev Protocol - Network/Socket Management
  IPv6 socket operations for Yggdrasil networks
}

unit BarevNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, BarevTypes, Process
  {$IFDEF UNIX}
  , BaseUnix, Unix, termio
  {$ENDIF}
  {$IFDEF WINDOWS}
  , WinSock2
  {$ENDIF};

const
  INET6_ADDRSTRLEN = 46; // Standard IPv6 address string length

type
  { Socket manager for handling connections }
  TBarevSocketManager = class
  private
    FListenSocket: TSocket;
    FListenSocket6: TSocket;
    FListenPort: Word;
    FOnLog: TLogEvent;
    procedure Log(const Level, Message: string);
  public
    constructor Create(APort: Word = BAREV_DEFAULT_PORT);
    destructor Destroy; override;

    { Listening socket operations }
    function StartListening: Boolean;
    procedure StopListening;
    //function AcceptConnection(out ClientAddr: string): TSocket;
    function AcceptConnection(out ClientAddr: string; out ClientPort: Word): TSocket;


    { Client connection operations }
    function ConnectTo(const IPv6Addr: string; Port: Word): TSocket;

    { Socket operations }
    function SetNonBlocking(Socket: TSocket): Boolean;
    function IsSocketReadable(Socket: TSocket; TimeoutMS: Integer = 0): Boolean;
    function IsSocketWritable(Socket: TSocket; TimeoutMS: Integer = 0): Boolean;

    { Data transfer }
    function SendData(Socket: TSocket; const Data: string): Integer;
    function ReceiveData(Socket: TSocket; out Data: string): Integer;

    { Properties }
    property ListenSocket: TSocket read FListenSocket;
    property ListenPort: Word read FListenPort;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

{ Helper functions }
function GetLocalYggdrasilIPs: TStringList;
function ResolveIPv6Address(const Address: string): string;

implementation

uses
  StrUtils;

{ TBarevSocketManager }

constructor TBarevSocketManager.Create(APort: Word);
begin
  inherited Create;
  FListenPort := APort;
  FListenSocket := -1;
  FListenSocket6 := -1;
end;

destructor TBarevSocketManager.Destroy;
begin
  StopListening;
  inherited;
end;

procedure TBarevSocketManager.Log(const Level, Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Level, Message);
end;

function TBarevSocketManager.StartListening: Boolean;
var
  Addr: TInetSockAddr6;
  OptVal: Integer;
begin
  Result := False;

  if FListenSocket6 <> -1 then
  begin
    Log('INFO', 'Already listening on port ' + IntToStr(FListenPort));
    Exit(True);
  end;

  // Create IPv6 socket
  FListenSocket6 := fpSocket(AF_INET6, SOCK_STREAM, 0);
  if FListenSocket6 < 0 then
  begin
    Log('ERROR', 'Failed to create IPv6 socket: ' + IntToStr(SocketError));
    Exit;
  end;

  // Set socket options
  OptVal := 1;
  fpSetSockOpt(FListenSocket6, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // Bind to IPv6 address
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin6_family := AF_INET6;
  Addr.sin6_port := htons(FListenPort);
  // Bind to all interfaces (IPv6 any address is all zeros)
  // Addr.sin6_addr is already zeroed by FillChar

  if fpBind(FListenSocket6, @Addr, SizeOf(Addr)) < 0 then
  begin
    Log('ERROR', 'Failed to bind to port ' + IntToStr(FListenPort) +
        ': ' + IntToStr(SocketError));
    CloseSocket(FListenSocket6);
    FListenSocket6 := -1;
    Exit;
  end;

  // Start listening
  if fpListen(FListenSocket6, 5) < 0 then
  begin
    Log('ERROR', 'Failed to listen on port ' + IntToStr(FListenPort) +
        ': ' + IntToStr(SocketError));
    CloseSocket(FListenSocket6);
    FListenSocket6 := -1;
    Exit;
  end;

  // Set non-blocking mode
  if not SetNonBlocking(FListenSocket6) then
  begin
    Log('WARN', 'Failed to set listen socket to non-blocking mode');
  end;

  FListenSocket := FListenSocket6;
  Log('INFO', 'Listening on port ' + IntToStr(FListenPort));
  Result := True;
end;

procedure TBarevSocketManager.StopListening;
begin
  if FListenSocket6 <> -1 then
  begin
    CloseSocket(FListenSocket6);
    FListenSocket6 := -1;
    FListenSocket := -1;
    Log('INFO', 'Stopped listening');
  end;
end;

function TBarevSocketManager.AcceptConnection(out ClientAddr: string; out ClientPort: Word): TSocket;
var
  Addr: TInetSockAddr6;
  AddrLen: TSockLen;
begin
  Result := -1;
  ClientAddr := '';
  ClientPort := 0;

  if FListenSocket6 = -1 then
  begin
    Log('ERROR', 'Not listening, cannot accept connections');
    Exit;
  end;

  AddrLen := SizeOf(Addr);
  FillChar(Addr, AddrLen, 0);

  Result := fpAccept(FListenSocket6, @Addr, @AddrLen);

  if Result < 0 then
  begin
    {$IFDEF UNIX}
    if (SocketError = ESysEAGAIN) or (SocketError = ESysEWOULDBLOCK) then
      Exit;
    {$ENDIF}

    Log('ERROR', 'Accept failed: ' + IntToStr(SocketError));
    Exit;
  end;

  ClientAddr := NetAddrToStr6(Addr.sin6_addr);
  ClientPort := ntohs(Addr.sin6_port);  // Extract port from network byte order

  if ClientAddr = '' then
    ClientAddr := '(unknown)';

  SetNonBlocking(Result);

  Log('INFO', 'Accepted connection from ' + ClientAddr + ':' + IntToStr(ClientPort));
end;

function TBarevSocketManager.ConnectTo(const IPv6Addr: string; Port: Word): TSocket;
var
  Addr: TInetSockAddr6;
  ConnectResult: Integer;
begin
  Result := -1;

  // Create socket
  Result := fpSocket(AF_INET6, SOCK_STREAM, 0);
  if Result < 0 then
  begin
    Log('ERROR', 'Failed to create socket for connection: ' + IntToStr(SocketError));
    Exit;
  end;

  // Set non-blocking before connect
  if not SetNonBlocking(Result) then
  begin
    Log('WARN', 'Failed to set socket to non-blocking before connect');
  end;

  // Prepare address structure
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin6_family := AF_INET6;
  Addr.sin6_port := htons(Port);

  // Convert IPv6 address string to network address using StrToNetAddr6
  Addr.sin6_addr := StrToNetAddr6(IPv6Addr);

  // Check if conversion was successful (check if result is all zeros and input wasn't '::')
  if (Addr.sin6_addr.u6_addr32[0] = 0) and
     (Addr.sin6_addr.u6_addr32[1] = 0) and
     (Addr.sin6_addr.u6_addr32[2] = 0) and
     (Addr.sin6_addr.u6_addr32[3] = 0) and
     (IPv6Addr <> '::') then
  begin
    Log('ERROR', 'Invalid IPv6 address: ' + IPv6Addr);
    CloseSocket(Result);
    Exit(-1);
  end;

  // Initiate connection (non-blocking, will return immediately)
  ConnectResult := fpConnect(Result, @Addr, SizeOf(Addr));

  if ConnectResult < 0 then
  begin
    {$IFDEF UNIX}
    // EINPROGRESS is expected for non-blocking connect
    if SocketError = ESysEINPROGRESS then
    begin
      Log('INFO', 'Connection to ' + IPv6Addr + ':' + IntToStr(Port) + ' in progress');
      Exit; // Return socket, caller will check for completion
    end;
    {$ENDIF}

    Log('ERROR', 'Failed to connect to ' + IPv6Addr + ':' + IntToStr(Port) +
        ': ' + IntToStr(SocketError));
    CloseSocket(Result);
    Exit(-1);
  end;

  Log('INFO', 'Connected to ' + IPv6Addr + ':' + IntToStr(Port));
end;

function TBarevSocketManager.SetNonBlocking(Socket: TSocket): Boolean;
{$IFDEF UNIX}
var
  Flags: Integer;
{$ENDIF}
{$IFDEF WINDOWS}
var
  NonBlock: u_long;
{$ENDIF}
begin
  Result := False;

  if Socket < 0 then Exit;

  {$IFDEF UNIX}
  Flags := FpFcntl(Socket, F_GETFL, 0);
  if Flags < 0 then Exit;

  Result := FpFcntl(Socket, F_SETFL, Flags or O_NONBLOCK) >= 0;
  {$ENDIF}

  {$IFDEF WINDOWS}
  NonBlock := 1;
  Result := ioctlsocket(Socket, FIONBIO, @NonBlock) = 0;
  {$ENDIF}
end;

function TBarevSocketManager.IsSocketReadable(Socket: TSocket; TimeoutMS: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: Integer;
begin
  Result := False;

  if Socket < 0 then Exit;

  {$IFDEF UNIX}
  fpFD_ZERO(FDSet);
  fpFD_SET(Socket, FDSet);

  TimeVal.tv_sec := TimeoutMS div 1000;
  TimeVal.tv_usec := (TimeoutMS mod 1000) * 1000;

  SelectResult := fpSelect(Socket + 1, @FDSet, nil, nil, @TimeVal);

  Result := (SelectResult > 0) and (fpFD_ISSET(Socket, FDSet) <> 0);
  {$ELSE}
  FD_ZERO(FDSet);
  FD_SET(Socket, FDSet);

  TimeVal.tv_sec := TimeoutMS div 1000;
  TimeVal.tv_usec := (TimeoutMS mod 1000) * 1000;

  SelectResult := WinSock2.select(Socket + 1, @FDSet, nil, nil, @TimeVal);

  Result := (SelectResult > 0) and FD_ISSET(Socket, FDSet);
  {$ENDIF}
end;

function TBarevSocketManager.IsSocketWritable(Socket: TSocket; TimeoutMS: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: Integer;
begin
  Result := False;

  if Socket < 0 then Exit;

  {$IFDEF UNIX}
  fpFD_ZERO(FDSet);
  fpFD_SET(Socket, FDSet);

  TimeVal.tv_sec := TimeoutMS div 1000;
  TimeVal.tv_usec := (TimeoutMS mod 1000) * 1000;

  SelectResult := fpSelect(Socket + 1, nil, @FDSet, nil, @TimeVal);

  Result := (SelectResult > 0) and (fpFD_ISSET(Socket, FDSet) <> 0);
  {$ELSE}
  FD_ZERO(FDSet);
  FD_SET(Socket, FDSet);

  TimeVal.tv_sec := TimeoutMS div 1000;
  TimeVal.tv_usec := (TimeoutMS mod 1000) * 1000;

  SelectResult := WinSock2.select(Socket + 1, nil, @FDSet, nil, @TimeVal);

  Result := (SelectResult > 0) and FD_ISSET(Socket, FDSet);
  {$ENDIF}
end;

function TBarevSocketManager.SendData(Socket: TSocket; const Data: string): Integer;
var
  BytesToSend: PChar;
  TotalSent: Integer;
  JustSent: Integer;
  DataLen: Integer;
begin
  Result := 0;
  TotalSent := 0;
  DataLen := Length(Data);

  if (Socket < 0) or (DataLen = 0) then Exit;

  BytesToSend := PChar(Data);

  while TotalSent < DataLen do
  begin
    JustSent := fpSend(Socket, BytesToSend + TotalSent, DataLen - TotalSent, 0);

    if JustSent < 0 then
    begin
      {$IFDEF UNIX}
      if (SocketError = ESysEAGAIN) or (SocketError = ESysEWOULDBLOCK) then
        Break; // Would block, return what we sent so far
      {$ENDIF}

      Log('ERROR', 'Send failed: ' + IntToStr(SocketError));
      Exit(-1);
    end
    else if JustSent = 0 then
      Break; // Connection closed

    Inc(TotalSent, JustSent);
  end;

  Result := TotalSent;
end;

function TBarevSocketManager.ReceiveData(Socket: TSocket; out Data: string): Integer;
var
  Buffer: array[0..RECV_BUFFER_SIZE-1] of Char;
  BytesRead: Integer;
begin
  Result := 0;
  Data := '';

  if Socket < 0 then Exit;

  BytesRead := fpRecv(Socket, @Buffer[0], RECV_BUFFER_SIZE, 0);

  if BytesRead < 0 then
  begin
    {$IFDEF UNIX}
    if (SocketError = ESysEAGAIN) or (SocketError = ESysEWOULDBLOCK) then
      Exit(0); // No data available, this is normal
    {$ENDIF}

    Log('ERROR', 'Receive failed: ' + IntToStr(SocketError));
    Exit(-1);
  end
  else if BytesRead = 0 then
  begin
    // Connection closed by peer
    Exit(0);
  end;

  SetString(Data, PChar(@Buffer[0]), BytesRead);
  Result := BytesRead;
end;

{ Helper functions }

function GetLocalYggdrasilIPs: TStringList;
{$IFDEF UNIX}
var
  Process: TProcess;
  OutputLines: TStringList;
  Line: string;
  i: Integer;
{$ENDIF}
begin
  Result := TStringList.Create;

  {$IFDEF UNIX}
  // Use 'ip -6 addr' to get IPv6 addresses
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    Process.Executable := 'ip';
    Process.Parameters.Add('-6');
    Process.Parameters.Add('addr');
    Process.Options := [poUsePipes, poWaitOnExit];

    try
      Process.Execute;
      OutputLines.LoadFromStream(Process.Output);

      for i := 0 to OutputLines.Count - 1 do
      begin
        Line := Trim(OutputLines[i]);
        // Look for lines with 'inet6' and check if they're Yggdrasil addresses
        if (Pos('inet6', Line) > 0) and (Pos('scope global', Line) > 0) then
        begin
          // Extract the address
          Line := Copy(Line, Pos('inet6', Line) + 6, Length(Line));
          Line := Trim(Line);
          if Pos('/', Line) > 0 then
            Line := Copy(Line, 1, Pos('/', Line) - 1);

          if IsYggdrasilAddress(Line) then
            Result.Add(Line);
        end;
      end;
    except
      // Silently fail if command doesn't work
    end;
  finally
    OutputLines.Free;
    Process.Free;
  end;
  {$ELSE}
  // Windows/other platforms would need different implementation
  {$ENDIF}
end;

function ResolveIPv6Address(const Address: string): string;
begin
  // For now, just return the address as-is
  // In future, could add DNS resolution
  Result := Address;
end;

end.
