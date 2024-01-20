---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum? 

HTTP-Anfrage mit Basic Authentication ermöglicht es Programmierern, einem Server Authentifizierungsdaten in einer HTTP-Anfrage zu übermitteln. Das ist nützlich, wenn wir Daten abrufen möchten, die nur für authentifizierte Benutzer zugänglich sind.

## Wie Man:

In Elixir können wir das HTTPoison-Paket verwenden, um HTTP-Anfragen zu senden und HTTParty für die Basic Authentication. Hier ist ein Beispiel:

```elixir
defmodule MyModule do
  def fetch_basic_auth_data do
    url = "https://api.meinedomain.de/resource"
    headers = ["Authorization": {:basic_auth, {"benutzername", "passwort"}}]

    HTTPoison.get(url, headers)
  end
end
```

Diese Funktion sendet eine GET-Anfrage an die angegebene URL mit Basic Authentication. Der Serverantwort kann dann wie folgt gehandhabt werden:

```elixir
case MyModule.fetch_basic_auth_data() do
  {:ok, response} ->
    IO.inspect(response.status_code) # Gibt den Statuscode der Antwort aus.
    IO.inspect(response.body) # Gibt den Inhalt der Antwort aus.

  {:error, reason} ->
    IO.inspect(reason) # Gibt den Grund für den Fehler aus.
end
```

## Deep Dive

Die Idee der Basic-Authentication stammt aus den Anfängen des Web. Sie war Teil der HTTP/1.0 Spezifikation und dient als einfacher Mechanismus zur Übermittlung von Benutzerdaten. Sie hat jedoch ihre Schwächen, insbesondere das Fehlen einer eingebauten Verschlüsselung.

Heutzutage gibt es sicherere Alternativen wie OAuth oder JWT (JSON Web Tokens), die auf den meisten modernen APIs zum Einsatz kommen. Sie bieten Verbesserungen wie die Möglichkeit zur Delegation von Berechtigungen oder zur zeitgesteuerten Ablaufsteuerung von Tokens.

Allerdings ist die Basic Authentication noch immer in vielen Situationen sinnvoll - insbesondere für einfachere Aufgaben oder beim Umgang mit älteren Systemen.

## Siehe Auch

Für weitere Informationen und alternative Implementierungsdetails, siehe die folgenden Ressourcen:

- [Elixir HTTPoison GitHub Repo](https://github.com/edgurgel/httpoison)
- [HTTParty GitHub Repo](https://github.com/jnunemaker/httparty)