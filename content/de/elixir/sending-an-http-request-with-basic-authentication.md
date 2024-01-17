---
title:                "Versenden einer HTTP-Anfrage mit Basic Authentication"
html_title:           "Elixir: Versenden einer HTTP-Anfrage mit Basic Authentication"
simple_title:         "Versenden einer HTTP-Anfrage mit Basic Authentication"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Senden einer HTTP-Anfrage mit Basisauthentifizierung ist eine gängige Methode für Programmierer, um eine Verbindung mit einem Webserver herzustellen und geschützte Endpunkte abzurufen. Um die Zugriffsebene zu kontrollieren, wird ein Benutzername und Passwort in der Anfrage verwendet, um die Identität des Absenders zu überprüfen.

Wie geht's?
```Elixir
  url = "https://example.com/api/users"
  username = "John"
  password = "123456"

  # Beginnen Sie Ihre Anfrage mit `HTTPBasicAuth`
  HTTPBasicAuth.request(:get, url, username, password)
```

In der obenstehenden Beispielanfrage wird die URL sowie der Benutzername und das Passwort in Variablen gespeichert. Mit dem Verwendung von `HTTPBasicAuth` wird eine Anfrage an den angegebenen Endpunkt gesendet, wobei die Basisauthentifizierung verwendet wird. Für eine POST-Anfrage können zusätzliche Optionen wie der Body oder die Header hinzugefügt werden.

Tiefentauchen
Die Basisauthentifizierung wurde erstmals im Jahr 1999 in der RFC 2617 spezifiziert und ist nach wie vor eine der beliebtesten Methoden, um sich an einem Webserver zu authentifizieren. Es gibt jedoch auch alternative Methoden wie OAuth oder JWT (JSON Web Tokens). Der Elixir-Standardbibliothek `HTTPoison` bietet eine einfache und benutzerfreundliche Möglichkeit, HTTP-Anfragen mit Basisauthentifizierung zu senden.

Siehe auch
- [RFC 2617](https://tools.ietf.org/html/rfc2617)
- [HTTPoison Dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html#module-http-basic-auth)