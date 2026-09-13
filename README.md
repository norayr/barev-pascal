# Barev Pascal Library

A FreePascal implementation of the Barev protocol - a simplified peer-to-peer XMPP protocol for Yggdrasil IPv6 networks.

## Overview

Barev enables direct messaging between nodes on Yggdrasil networks without central servers, using a "one pipe per buddy" connection model.

- **JID Format**: `localpart@yggdrasil_ipv6_address`
- **Port**: 1377 (default, configurable)
- **Protocol**: TCP over IPv6 (Yggdrasil addresses only)
- **Security**: Provided by Yggdrasil network encryption

## Project Structure

```
barevtypes.pas  - Basic types, constants, and data structures
barevxml.pas    - XML generation and parsing for XMPP stanzas
barevnet.pas    - Network/socket management (IPv6)
barev.pas       - Main client interface
testbarev.pas   - Simple test/demo program
```

## Requirements

- FreePascal 3.0.0 or later
- Linux/Unix system (uses Unix sockets API)
- Yggdrasil network connection with IPv6 address

## Building

### Quick Build

```bash
make
```

This creates the `testbarev` executable.

### Manual Build

```bash
fpc testbarev.pas
```

### Building Individual Units

```bash
make install-units
```

This compiles all units into the `lib/` directory.

## Usage

### Simple Example

```pascal
uses
  Barev, BarevTypes;

var
  Client: TBarevClient;
  Buddy: TBarevBuddy;

begin
  // Create client with your nick and Yggdrasil IPv6
  Client := TBarevClient.Create('mynick', '201:af82:9f2f:7809::1');

  // Set up event handlers
  Client.OnMessageReceived := @OnMessageReceived;
  Client.OnBuddyStatus := @OnBuddyStatus;
  Client.OnLog := @OnLog;

  // Start listening
  if not Client.Start then
    WriteLn('Failed to start');

  // Add a buddy
  Buddy := Client.AddBuddy('friend', '201:7a74:aa1e:101a::a1');

  // Connect to buddy
  Client.ConnectToBuddy(Buddy.JID);

  // Main loop
  while Running do
  begin
    Client.Process;  // Handle network events
    // Your application logic here
    Sleep(100);
  end;

  // Cleanup
  Client.Stop;
  Client.Free;
end.
```

### Running the Test Program

```bash
./testbarev
```

The test program will prompt for:
1. Your nickname
2. Your Yggdrasil IPv6 address

Then provides an interactive command-line interface.

### Test Program Commands

```
help                   - Show help
add <nick@ipv6>        - Add a buddy
list                   - List all buddies
connect <nick@ipv6>    - Connect to a buddy
msg <nick@ipv6> <text> - Send a message
status <status> [msg]  - Set your status (available/away/dnd)
load <file>            - Load contacts from file
save <file>            - Save contacts to file
quit                   - Exit
```

### Contact List File Format

Create a text file (e.g., `contacts.txt`):

```
# Barev Contacts
# Format: nick@ipv6_address or nick@ipv6_address:port

alice@201:af82:9f2f:7809:be0c:360a:1587:6be7
bob@201:7a74:aa1e:101a::a1:5299
charlie@202:baad:cafe:1234::1
```

Then load it:
```
> load contacts.txt
```

## API Reference

### TBarevClient

Main client class for Barev messaging.

#### Constructor
```pascal
constructor Create(const ANick, AMyIPv6: string; APort: Word = BAREV_DEFAULT_PORT);
```

#### Methods

**Client Control**
- `Start: Boolean` - Start the client and begin listening
- `Stop` - Stop the client
- `Process` - Process network events (call regularly in your main loop)

**Buddy Management**
- `AddBuddy(Nick, IPv6: string; Port: Word): TBarevBuddy` - Add a buddy
- `RemoveBuddy(JID: string): Boolean` - Remove a buddy
- `GetBuddy(JID: string): TBarevBuddy` - Find buddy by JID
- `GetBuddyCount: Integer` - Get number of buddies
- `GetBuddyByIndex(Index: Integer): TBarevBuddy` - Get buddy by index

**Contact List**
- `LoadContactsFromFile(FileName: string): Boolean` - Load contacts
- `SaveContactsToFile(FileName: string): Boolean` - Save contacts

**Communication**
- `ConnectToBuddy(JID: string): Boolean` - Initiate connection to buddy
- `SendMessage(JID, Text: string): Boolean` - Send a message
- `SendPresence(Status: TBuddyStatus; Message: string): Boolean` - Broadcast presence
- `SendPresenceToBuddy(JID: string; Status: TBuddyStatus; Message: string): Boolean` - Send presence to specific buddy

#### Event Handlers

```pascal
property OnBuddyStatus: TBuddyStatusEvent;
  // Called when buddy status changes
  // procedure(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus)

property OnMessageReceived: TMessageReceivedEvent;
  // Called when message is received
  // procedure(Buddy: TBarevBuddy; const MessageText: string)

property OnConnectionState: TConnectionStateEvent;
  // Called when connection state changes
  // procedure(Buddy: TBarevBuddy; State: TConnectionState)

property OnLog: TLogEvent;
  // Called for log messages
  // procedure(const LogLevel, Message: string)
```

### TBarevBuddy

Represents a contact/buddy.

#### Properties
- `Nick: string` - Buddy's nickname
- `IPv6Address: string` - Buddy's Yggdrasil IPv6 address
- `Port: Word` - Buddy's port number
- `JID: string` - Full JID (nick@ipv6)
- `Status: TBuddyStatus` - Current status
- `StatusMessage: string` - Status message

### Types

#### TBuddyStatus
```pascal
bsOffline
bsAvailable
bsAway
bsExtendedAway
bsDoNotDisturb
```

#### TConnectionState
```pascal
csDisconnected
csConnecting
csStreamInit
csAuthenticated
csOnline
```

## Testing Two Clients

### Terminal 1:
```bash
./testbarev
Enter your nick: alice
Enter your Yggdrasil IPv6: 201:af82:9f2f:7809::1

> add bob@201:7a74:aa1e:101a::a1
> connect bob@201:7a74:aa1e:101a::a1
```

### Terminal 2:
```bash
./testbarev
Enter your nick: bob
Enter your Yggdrasil IPv6: 201:7a74:aa1e:101a::a1

> add alice@201:af82:9f2f:7809::1
# Alice's connection should be accepted automatically
```

### Send Messages:
```bash
# In Terminal 1:
> msg bob@201:7a74:aa1e:101a::a1 Hello Bob!

# In Terminal 2:
*** Message from alice: Hello Bob!
> msg alice@201:af82:9f2f:7809::1 Hi Alice!
```

## Protocol Details

### Connection Model

- Each buddy pair maintains exactly ONE active TCP connection
- First connection established wins
- All bidirectional communication flows through this single pipe
- Subsequent connections from the same buddy replace the existing connection

### Stream Flow

1. **Initiator** sends stream header:
```xml
<?xml version="1.0" encoding="UTF-8" ?>
<stream:stream xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" from="you@your_ip" to="buddy@buddy_ip">
```

2. **Receiver** responds with their stream header:
```xml
<?xml version="1.0" encoding="UTF-8" ?>
<stream:stream xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" from="buddy@buddy_ip" to="you@your_ip">
```

3. Both exchange presence:
```xml
<presence/>
```

4. Chat communication begins

### Message Format

```xml
<message to="buddy@buddy_ip" type="chat">
  <body>Hello world!</body>
</message>
```

### Keepalive (Ping/Pong)

The library automatically sends pings every 30 seconds and marks buddies offline after 3 consecutive failures.

## Limitations & TODO

**Current Limitations:**
- No file transfer support (XEP-0096/XEP-0065 not implemented)
- No VCard support
- Simple XML parser (not full XMPP compliance)
- Unix/Linux only (socket API)
- No TLS (relies on Yggdrasil encryption)

**Future Enhancements:**
- File transfer support
- VCard/avatar support
- Better XML parsing (full DOM or SAX)
- Windows socket support
- Threading for better concurrency
- Logging levels
- Configuration file support
- Message history

## Troubleshooting

### "Failed to bind to port 5299"
- Port might already be in use
- Try a different port: `Client := TBarevClient.Create('nick', 'ipv6', 5300);`
- Check with: `netstat -tuln | grep 5299`

### "Connection refused"
- Buddy is not listening (not running Barev client)
- Wrong IPv6 address
- Firewall blocking port 5299
- Test connectivity: `ping6 <buddy_ipv6>`

### "Failed to connect: address resolution failed"
- Invalid IPv6 address format
- Not a Yggdrasil address (must start with 200-3ff)

### "Connection from unknown IP"
- Buddy not in your contact list
- Add them first: `add <nick@ipv6>`

## Contributing

This is an initial implementation. Improvements welcome!

Key areas for contribution:
- Windows support
- Better threading/async I/O
- Full XMPP compliance
- File transfer support
- Unit tests

## License

GPL v2 or later (matching original Pidgin Bonjour plugin)

## Credits

Based on the Pidgin Bonjour plugin, modified for Yggdrasil networks.
Protocol specification: barev.md

## References

- Barev Protocol: https://github.com/norayr/barev
- Yggdrasil Network: https://yggdrasil-network.github.io/
- XMPP Core: RFC 6120
- Pidgin: https://pidgin.im/
