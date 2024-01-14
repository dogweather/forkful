---
title:                "Bash: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist ein wichtiger Schritt in der Programmierung mit Bash, um sicherzustellen, dass die Daten, die über das Internet gesendet werden, geschützt sind und nur für autorisierte Benutzer zugänglich sind.

## Wie man es macht

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Bash zu senden, müssen Sie bestimmte Schritte befolgen:

1. Definieren Sie die Variablen für die URL, den Benutzernamen und das Passwort:

```Bash
URL="www.example.com"
USER="username"
PASS="password"
```

2. Verwenden Sie den Befehl "curl", um eine GET-Anfrage an die URL mit der Authentifizierungsinformation zu senden:

```Bash
curl -u $USER:$PASS $URL
```

3. Verwenden Sie den Befehl "curl" mit der Option "-X", um eine POST-Anfrage zu senden:

```Bash
curl -u $USER:$PASS -X POST -d 'data=example' $URL
```

Das Ergebnis dieser Anfragen wird in der Terminalausgabe angezeigt.

## Tiefere Einblicke

Die grundlegende Authentifizierung funktioniert durch die Übermittlung von Benutzernamen und Passwort als Teil der HTTP-Anfrage. Dies ist eine einfache und weit verbreitete Methode, um den Zugriff auf Webressourcen zu beschränken. Der Benutzername und das Passwort werden in der HTTP-Header-Autorisierungsbereich übergeben, im Format "Benutzer:Passwort" base64-codiert. Es ist wichtig zu beachten, dass diese Methode nicht die sicherste ist und daher sollten zusätzliche Sicherheitsmaßnahmen wie HTTPS verwendet werden.

## Siehe Auch

- [Offizielle Dokumentation zu cURL in Bash](https://curl.haxx.se/docs/manpage.html)
- [HTTP-Authentifizierungsmechanismen im Detail](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)