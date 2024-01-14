---
title:                "Fish Shell: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Oftmals müssen wir als Entwicklerinnen und Entwickler mit APIs arbeiten, die eine Authentifizierung erfordern. Basic Authentication ist dabei eine einfache und weit verbreitete Methode, um Zugriff auf bestimmte Ressourcen zu erhalten. In diesem Blog-Beitrag werden wir sehen, wie man mit der Fish Shell eine HTTP-Anfrage mit Basic Authentication senden kann.

## Wie geht das?

Wir werden die `curl`-Befehle der Fish Shell verwenden, um eine POST-Anfrage an die API zu senden. Dazu müssen wir zuerst unseren Benutzernamen und unser Passwort in einer bestimmten Formatierung angeben:

```Fish Shell
set username "username"
set password "password"
set credentials (printf "%s:%s" $username $password | base64)
```

Mit dem `set`-Befehl legen wir unsere Benutzerdaten als Variablen fest. Dann verwenden wir `base64`, um die Daten in ein bestimmtes Format zu bringen, das für Basic Authentication erforderlich ist.

Nun können wir unsere HTTP-Anfrage senden, indem wir die `curl`-Befehle verwenden:

```Fish Shell
curl -H "Authorization: Basic $credentials" -X POST https://example.com/api/endpoint
```

Wir setzen dabei die `Authorization`-Header auf unsere zuvor erstellten Credentials und geben die URL an, an die wir die Anfrage senden möchten.

Das war's schon! Wenn alles richtig gemacht wurde, sollten wir eine erfolgreiche Antwort von der API erhalten.

## Tiefer Einblick

Basic Authentication verwendet das Base64-Format, um Benutzerdaten zu übermitteln. Dabei werden Benutzername und Passwort einfach zusammengefügt und dann kodiert. Dies ist jedoch keine sichere Methode, da Base64 leicht wieder in Klartext umgewandelt werden kann. Aus diesem Grund wird empfohlen, für sensiblere Anfragen eine sicherere Authentifizierungsmethode zu verwenden.

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [curl Befehlsreferenz](https://curl.haxx.se/docs/manpage.html)
- [Base64 Erklärung](https://de.wikipedia.org/wiki/Base64)