library barev_c_api;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, ctypes,
  Barev, BarevTypes;

type
  PBarevClient = ^TBarevClient;

function barev_strdup(const s: string): PChar; cdecl;
begin
  // Caller must free with barev_strfree
  Result := StrNew(PChar(s));
end;

procedure barev_strfree(p: PChar); cdecl;
begin
  if p <> nil then
    StrDispose(p);
end;

function barev_client_new(nick, myipv6: PChar; port: cuint16): Pointer; cdecl;
var
  c: TBarevClient;
begin
  try
    c := TBarevClient.Create(string(nick), string(myipv6), port);
    Result := Pointer(c);
  except
    Result := nil;
  end;
end;

procedure barev_client_free(h: Pointer); cdecl;
begin
  if h <> nil then
    TBarevClient(h).Free;
end;

function barev_client_start(h: Pointer): cint; cdecl;
begin
  if (h <> nil) and TBarevClient(h).Start then Result := 1 else Result := 0;
end;

procedure barev_client_stop(h: Pointer); cdecl;
begin
  if h <> nil then
    TBarevClient(h).Stop;
end;

procedure barev_client_process(h: Pointer); cdecl;
begin
  if h <> nil then
    TBarevClient(h).Process;
end;

function barev_client_myjid(h: Pointer): PChar; cdecl;
begin
  if h = nil then Exit(nil);
  Result := barev_strdup(TBarevClient(h).MyJID);
end;

function barev_client_add_buddy(h: Pointer; buddynick, buddyipv6: PChar; port: cuint16): PChar; cdecl;
var
  b: TBarevBuddy;
begin
  if h = nil then Exit(nil);
  b := TBarevClient(h).AddBuddy(string(buddynick), string(buddyipv6), port);
  if b = nil then Exit(nil);
  // Return Buddy JID as newly allocated C string
  Result := barev_strdup(b.JID);
end;

function barev_client_connect(h: Pointer; buddyjid: PChar): cint; cdecl;
begin
  if (h <> nil) and TBarevClient(h).ConnectToBuddy(string(buddyjid)) then Result := 1 else Result := 0;
end;

function barev_client_send_message(h: Pointer; buddyjid, msg: PChar): cint; cdecl;
begin
  if (h <> nil) and TBarevClient(h).SendMessage(string(buddyjid), string(msg)) then Result := 1 else Result := 0;
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
  barev_strfree name 'barev_strfree';

begin
end.

