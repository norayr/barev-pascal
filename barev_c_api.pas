library barev_capi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, ctypes,
  Barev, BarevTypes;

type
  // C callback style: (buddy_jid, isTyping, userdata)
  TBarevTypingCB = procedure(buddy_jid: PChar; isTyping: cint; userdata: Pointer); cdecl;

  // Bridge object: method matches "of object" signature
  TTypingBridge = class
  public
    TypingCB: TBarevTypingCB;
    UserData: Pointer;
    procedure OnTyping(Buddy: TBarevBuddy; IsTyping: Boolean);
  end;

  PClientHandle = ^TClientHandle;
  TClientHandle = record
    Client: TBarevClient;
    Bridge: TTypingBridge;
  end;

procedure TTypingBridge.OnTyping(Buddy: TBarevBuddy; IsTyping: Boolean);
var
  jid: AnsiString;
begin
  if not Assigned(TypingCB) then Exit;

  // Keep an AnsiString alive for the duration of the call
  jid := AnsiString(Buddy.JID);
  TypingCB(PChar(jid), Ord(IsTyping), UserData);
end;

function barev_strdup(const s: string): PChar; cdecl;
begin
  Result := StrNew(PChar(s));
end;

procedure barev_strfree(p: PChar); cdecl;
begin
  if p <> nil then StrDispose(p);
end;

function barev_client_new(nick, myipv6: PChar; port: cuint16): Pointer; cdecl;
var
  h: PClientHandle;
begin
  Result := nil;
  New(h);
  FillChar(h^, SizeOf(h^), 0);
  try
    h^.Bridge := TTypingBridge.Create;
    h^.Bridge.TypingCB := nil;
    h^.Bridge.UserData := nil;

    h^.Client := TBarevClient.Create(string(nick), string(myipv6), port);

    // Initially no callback installed => keep it nil
    h^.Client.OnTypingNotification := nil;

    Result := Pointer(h);
  except
    if h <> nil then
    begin
      if h^.Client <> nil then h^.Client.Free;
      if h^.Bridge <> nil then h^.Bridge.Free;
      Dispose(h);
    end;
    Result := nil;
  end;
end;

procedure barev_client_free(handle: Pointer); cdecl;
var
  h: PClientHandle;
begin
  if handle = nil then Exit;
  h := PClientHandle(handle);

  if h^.Client <> nil then
  begin
    h^.Client.OnTypingNotification := nil;
    h^.Client.Free;
  end;

  if h^.Bridge <> nil then
    h^.Bridge.Free;

  Dispose(h);
end;

function barev_client_start(handle: Pointer): cint; cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit(0);
  h := PClientHandle(handle);
  if (h^.Client <> nil) and h^.Client.Start then Result := 1 else Result := 0;
end;

procedure barev_client_stop(handle: Pointer); cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit;
  h := PClientHandle(handle);
  if h^.Client <> nil then h^.Client.Stop;
end;

procedure barev_client_process(handle: Pointer); cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit;
  h := PClientHandle(handle);
  if h^.Client <> nil then h^.Client.Process;
end;

function barev_client_myjid(handle: Pointer): PChar; cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit(nil);
  h := PClientHandle(handle);
  if h^.Client = nil then Exit(nil);
  Result := barev_strdup(h^.Client.MyJID);
end;

function barev_client_add_buddy(handle: Pointer; buddynick, buddyipv6: PChar; port: cuint16): PChar; cdecl;
var
  h: PClientHandle;
  b: TBarevBuddy;
begin
  if handle = nil then Exit(nil);
  h := PClientHandle(handle);
  if h^.Client = nil then Exit(nil);

  b := h^.Client.AddBuddy(string(buddynick), string(buddyipv6), port);
  if b = nil then Exit(nil);
  Result := barev_strdup(b.JID);
end;

function barev_client_connect(handle: Pointer; buddyjid: PChar): cint; cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit(0);
  h := PClientHandle(handle);
  if (h^.Client <> nil) and h^.Client.ConnectToBuddy(string(buddyjid)) then Result := 1 else Result := 0;
end;

function barev_client_send_message(handle: Pointer; buddyjid, msg: PChar): cint; cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit(0);
  h := PClientHandle(handle);
  if (h^.Client <> nil) and h^.Client.SendMessage(string(buddyjid), string(msg)) then Result := 1 else Result := 0;
end;

procedure barev_set_typing_callback(handle: Pointer; cb: TBarevTypingCB; userdata: Pointer); cdecl;
var h: PClientHandle;
begin
  if handle = nil then Exit;
  h := PClientHandle(handle);
  if (h^.Client = nil) or (h^.Bridge = nil) then Exit;

  h^.Bridge.TypingCB := cb;
  h^.Bridge.UserData := userdata;

  if Assigned(cb) then
    h^.Client.OnTypingNotification := @h^.Bridge.OnTyping
  else
    h^.Client.OnTypingNotification := nil;
end;

exports
  barev_client_new name 'barev_client_new',
  barev_client_free name 'barev_client_free',
  barev_client_start name 'barev_client_start',
  barev_client_stop name 'barev_client_stop',
  barev_client_process name 'barev_client_process',
  barev_client_myjid name 'barev_client_myjid',
  barev_client_add_buddy name 'barev_client_add_buddy',
  barev_client_connect name 'barev_client_connect',
  barev_client_send_message name 'barev_client_send_message',
  barev_set_typing_callback name 'barev_set_typing_callback',
  barev_strfree name 'barev_strfree';

begin
end.
