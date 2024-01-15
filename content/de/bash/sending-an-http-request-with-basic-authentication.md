---
title:                "HTTP-Anfrage mit grundlegender Authentifizierung senden."
html_title:           "Bash: HTTP-Anfrage mit grundlegender Authentifizierung senden."
simple_title:         "HTTP-Anfrage mit grundlegender Authentifizierung senden."
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Wenn man eine Webanwendung oder API nutzt, die mit Benutzernamen und Passwort geschützt ist, ist es wichtig, eine HTTP-Anfrage mit Basic Authentication zu senden, um sich als gültiger Benutzer zu identifizieren.

## Wie geht's

Um eine HTTP-Anfrage mit Basic Authentication zu senden, muss man folgende Schritte befolgen:

1. Erstelle eine Variable mit dem Benutzernamen und eine mit dem Passwort.
2. Encode die Variable mit Base64, um sie sicher zu übertragen.
3. Füge den Header "Authorization" mit dem Wert "Basic <encoded_variable>" zur HTTP-Anfrage hinzu.
4. Sende die Anfrage ab und empfange die Antwort.

```Bash
username="benutzername"
password="passwort"
encoded_auth=$(echo -n "$username:$password" | base64)
curl -H "Authorization: Basic $encoded_auth" <url>
```

Die Antwort sollte den Daten entsprechen, die man normalerweise erhalten hätte, wenn man die Anfrage ohne Basic Authentication gesendet hätte.

## Tiefer eintauchen

Wenn man mehr über Basic Authentication erfahren möchte, kann man sich genauer mit dem RFC 7617 (https://tools.ietf.org/html/rfc7617) beschäftigen, welcher die genauen Spezifikationen für die Verwendung von Basic Authentication festlegt.

## Siehe auch

- https://www.baeldung.com/linux/curl-basic-authentication
- https://curl.se/docs/auth.html
- https://developer.mozilla.org/de/docs/Web/HTTP/Authentication#http_basic_authentication