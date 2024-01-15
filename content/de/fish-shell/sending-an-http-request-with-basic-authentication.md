---
title:                "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
html_title:           "Fish Shell: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Möglicherweise müssen Sie eine Anfrage an eine Website senden, die eine Basisauthentifizierung erfordert, um auf geschützte Inhalte zuzugreifen. Oder Sie möchten Ihre eigene Website vor unbefugtem Zugriff schützen, indem Sie Benutzernamen und Passwörter verwenden. In beiden Fällen ist es wichtig zu wissen, wie man in Fish Shell eine HTTP-Anfrage mit Basisauthentifizierung sendet.

## Wie geht es

```Fish Shell
set -l username <BENUTZERNAME>
set -l password <PASSWORT>

curl -u $username:$password <URL>
```

Dieser Codeblock zeigt, wie Sie die Variablen "username" und "password" deklarieren und dann curl mit dem "-u" Flag verwenden, um eine HTTP-Anfrage mit Basisauthentifizierung zu senden. Ersetzen Sie <BENUTZERNAME>, <PASSWORT> und <URL> durch die entsprechenden Werte.

Weitere Optionen von "curl" für HTTP-Anfragen mit Basisauthentifizierung können mit dem Befehl "curl --help" abgerufen werden.

## Tiefentauchen

Basisauthentifizierung ist eine der ältesten Methoden der HTTP-Authentifizierung. Sie erfordert die Angabe eines Benutzernamens und Passworts in Klartext als Teil der HTTP-Anfrage. Das macht es sehr unsicher, da es leicht von Dritten abgefangen werden kann.

Eine sicherere Alternative wäre die Verwendung von HTTPS oder die Verwendung von anderen Arten der Authentifizierung wie z.B. Digest oder OAuth.

## Siehe auch

- [Curl: Verwendung von Benutzername und Passwort für Authentifizierung](https://curl.haxx.se/docs/manpage.html)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [HTTP-Anforderungen und Authentifizierung](https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2)