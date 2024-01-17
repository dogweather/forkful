---
title:                "Eine http-Anfrage mit grundlegender Authentifizierung senden."
html_title:           "Bash: Eine http-Anfrage mit grundlegender Authentifizierung senden."
simple_title:         "Eine http-Anfrage mit grundlegender Authentifizierung senden."
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Senden einer HTTP-Anfrage mit Basic-Authentifizierung handelt es sich um einen Prozess, bei dem ein Programmierer eine angeforderte Ressource von einem Server anfordert und sich dabei mit Benutzername und Passwort authentifiziert. Dies ermöglicht die sichere Kommunikation zwischen einem Client und einem Server.

## Wie geht's?
```Bash
curl -u username:password http://www.example.com
```
Dieses Beispiel zeigt, wie man eine HTTP-Anfrage mit Basic-Authentifizierung mithilfe von cURL senden kann. Der Parameter -u ermöglicht die Angabe von Benutzername und Passwort, die dann zusammen mit der Anfrage an den Server geschickt werden.

Die Ausgabe kann je nach Anfrage variieren, sollte jedoch im Allgemeinen eine Antwort vom Server enthalten, die bestätigt, dass der Authentifizierungsprozess erfolgreich war.

## Tiefer eintauchen
### Historischer Kontext
Die Verwendung von Basic-Authentifizierung geht zurück auf die Anfänge des World Wide Web, als die Sicherheitsstandards noch nicht so hoch waren wie heute. Heutzutage wird es häufig durch sicherere Alternativen wie Digest-Authentifizierung oder OAuth ersetzt.

### Alternativen
Wie bereits erwähnt, gibt es sichere Alternativen zur Basic-Authentifizierung, die heutzutage empfohlen werden. Es ist wichtig, die Sicherheitsanforderungen des Projekts zu berücksichtigen und die entsprechende Authentifizierungsmethode zu wählen.

### Implementierungsdetails
Um eine HTTP-Anfrage mit Basic-Authentifizierung zu senden, muss der Benutzername und das Passwort im Header der Anfrage angegeben werden. Der Header sollte wie folgt aussehen:
```Bash
Authorization: Basic <Base64 encoded(username:password)>
```
Der Server entschlüsselt dann die Informationen und führt den Authentifizierungsprozess durch.

## Siehe auch
- [cURL Documentation](https://curl.haxx.se/docs/)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)