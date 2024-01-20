---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-Anfragen mit Basisauthentifizierung in Bash senden

## Was und Warum?

Eine HTTP-Anfrage mit Basisauthentifizierung ist eine Methode, mit der Benutzeranmeldedaten in einer HTTP-Anfrage gesendet werden. Entwickler nutzen das, um auf Daten und Funktionen geschützter Webdienste zuzugreifen.

## Wie geht das?

In Bash wird meistens das Programm `curl` genutzt, um HTTP-Anfragen zu senden, da es direkt in den meisten Unix-basierten Systemen verfügbar ist.

```Bash
curl -u benutzername:passwort http://example.com
```

Hierbei steht `-u` für "User". Der Doppelpunkt (`:`) trennt Benutzernamen und Passwort. 

Eine Antwort könnte programmatisch so aussehen:

```Bash
HTTP/1.1 200 OK
```

## Tiefer Tauchen

**Historischer Kontext:** Basisauthentifizierung wurde in der anfänglichen Spezifikation von HTTP, RFC 1945 eingeführt, und in RFC 2617 erweitert. Es verwendet Base64-Codierung, um Benutzername und Passwort zu maskieren, bietet jedoch keine echte Sicherheit.

**Alternativen:** Token-basierte Authentifizierungsmethoden wie OAuth und JWT sind heutzutage weit verbreitet, bieten aber größere Komplexität. Bei HTTPS-Übertragungen ist Basic auth jedoch ausreichend.

**Implementierung:** In `curl` nutzt die `-u` Flagge die Basic Auth-Methode. Bei ungesicherten Verbindungen kann ein Mittelsmann-Angriff ("man-in-the-middle") die Anmeldedaten abfangen.

## Siehe auch

- cURL-Befehl [Dokumentation](https://curl.haxx.se/docs/manual.html)
- RFC 1945, [HTTP/1.0 Spezifikation](https://tools.ietf.org/html/rfc1945)
- RFC 2617, [HTTP-Authentifizierung](https://tools.ietf.org/html/rfc2617)

Bitte beachten Sie die Sicherheitsrisiken bei Verwendung der Basisauthentifizierung und überprüfen Sie die Alternativen je nach Ihren Anwendungsfällen.