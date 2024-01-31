---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:28.229106-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Requests mit Basisauthentifizierung ermöglichen gesicherte Zugriffe auf Webressourcen, indem Benutzername und Passwort übermittelt werden. Programmierer nutzen dies, um APIs oder Webdienste zu konsumieren, die eine solche Authentifizierungsform erfordern.

## How to:
Elixir verwendet das `HTTPoison` Paket, ein beliebter HTTP-Client, um Requests mit Basisauthentifizierung durchzuführen. Installiere `HTTPoison` über `mix.exs` und folge dem Beispiel:

```elixir
# mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Jetzt in deinem Code:

```elixir
defmodule HTTPExample do
  def fetch_protected_content do
    # Benutzername und Passwort
    auth = {"meinBenutzername", "meinPasswort"}

    # HTTP GET Request mit Basic Authentication
    HTTPoison.get("https://meineprotectedseite.com", [], basic_auth: auth)
  end
end
```

Führe `mix deps.get` aus, um `HTTPoison` zu installieren, und dann rufe `HTTPExample.fetch_protected_content()` auf. Du erhältst eine Antwort im Format:

```elixir
{:ok, %HTTPoison.Response{status_code: 200, body: response_body}}
```

## Deep Dive
Die Basisauthentifizierung sendet Benutzernamen und Passwort im `Authorization`-Header, kodiert als Base64-String. Historisch wurde sie eingeführt, um eine einfache Zugangskontrolle zu implementieren. Obwohl einfach, gilt sie als unsicher, wenn nicht über HTTPS genutzt, da die Zugangsdaten im Klartext vorlagen könnten.

Alternativen zur Basisauthentifizierung beinhalten OAuth, Token-basierte Authentifizierung und andere komplexere Systeme, die zusätzliche Sicherheit bieten. 

In Elixir gibt es neben `HTTPoison` verschiedene Pakete wie `Tesla` oder `Hackney`, die ebenfalls HTTP-Requests ermöglichen. Die `HTTPoison`-Bibliothek basiert auf `Hackney`. Die Wahl hängt vom spezifischen Anwendungsfall ab.

## See Also
- [HTTPoison Git Repository](https://github.com/edgurgel/httpoison)
- [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [MDN Documentation on HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
