---
title:                "Versenden einer HTTP-Anfrage mit grundlegender Authentifizierung"
html_title:           "C++: Versenden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Du hast vielleicht schon mal von HTTP-Anfragen gehört, aber wusstest du auch, dass du diese Anfragen mit grundlegender Authentifizierung versehen kannst? Dieser Artikel erklärt, warum und wie du das machen kannst.

## Wie geht das

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, musst du Folgendes tun:

1. Definiere die Benutzername und Passwort Kombination als Basis64-kodierten String.
```C++
string credentials = base64_encode(username + ":" + password);
```

2. Erstelle eine Objektinstanz von `std::stringstream` und schreibe die HTTP-Anfrage in das Objekt.
```C++
stringstream request;
request << "GET /example HTTP/1.1\r\n";
request << "Host: example.com\r\n";
request << "Authorization: Basic " << credentials << "\r\n\r\n";
```

3. Erstelle eine Socket-Verbindung zu dem Server, zu dem du die Anfrage senden möchtest.
```C++
int sock = socket(AF_INET, SOCK_STREAM, 0);

struct sockaddr_in server;
server.sin_addr.s_addr = inet_addr("127.0.0.1");
server.sin_family = AF_INET;
server.sin_port = hton16(80);
```

4. Sende die Anfrage an den Server.
```C++
int bytesSent = send(sock, request.str().c_str(), request.str().length(), 0);
```

5. Warte auf eine Antwort vom Server und lese sie aus.
```C++
char response[4096];
int bytesReceived = recv(sock, response, 4096, 0);
```

6. Schließe die Socket-Verbindung zu dem Server.
```C++
closesocket(sock);
```

## Tiefer Einblick

Die grundlegende Authentifizierung in HTTP basiert auf einem mit Base64 kodierten Benutzername und Passwort, die mit dem Prefix "Basic" versehen werden. Der Server sucht dann in seinen Benutzerdatenbanken nach dieser Kombination und genehmigt die Anfrage, wenn sie übereinstimmt.

Das Problem bei dieser Methode ist, dass der Base64-kodierte String leicht zu entschlüsseln ist und eine einfache Methode darstellt, um Zugriff zu erhalten. Aus diesem Grund ist es wichtig, die Verbindung zwischen Client und Server verschlüsselt zu halten, um sicherzustellen, dass das Passwort nicht kompromittiert wird.

## Siehe auch

- [HTTP Basic Authentication in C++ Code](https://www.geeksforgeeks.org/http-basic-authentication-cpp-code/)
- [Basic Authentication in C++ with libcurl](https://notfalse.net/7/basic-authentication-cpp-libcurl)