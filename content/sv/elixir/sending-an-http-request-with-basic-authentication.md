---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran med grundläggande autentisering innebär att skicka en begäran från en klient till en server och inkluderar autentiseringsinformation i HTTP-header. Detta görs för att säkerställa att endast befogenheter mottagare har åtkomst till den skickade informationen.

## Hur man:

För att skicka en HTTP-begäran med grundläggande autentisering i Elixir, behöver vi använda HTTPoison-biblioteket. Och så här gör du:

```Elixir 
defmodule MyApp do
  def send_request do
    auth = {"username", "password"}
    headers = [basic_auth: auth]
    HTTPoison.get("http://example.com", headers)
  end
end
```
När den körs, skickar detta exempel en GET-begäran till ”http://example.com” och inkluderar grundläggande autentiseringsinformation (”username” och ”password”) i HTTP-huvudet.

## Fördjupning:

Historiskt sett utvecklades grundläggande autentisering för HTTP/1.0 som ett sätt att skydda information under överföringen. Eftersom det använder Base64-kodning, ger det emellertid ingen verklig säkerhet utan kryptering.

Alternativ till grundläggande autentisering inkluderar Digest Authentication, en mer komplex men säkrare metod som använder MD5-kryptering, och OAuth, som tillåter autentisering utan att låsa lösenord.

När vi skickar en HTTP-begäran med grundläggande autentisering i Elixir, sker detta i två steg: först skapas HTTP-headern med autentiseringsuppgifterna, sedan skickas själva begäran med den headern.

## Se även:

- HTTPoison Documentation: https://hexdocs.pm/httpoison/HTTPoison.html 
- Wikipedia on Basic Authentication: https://sv.wikipedia.org/wiki/Basic_access_authentication 
- Digest Authentication: https://sv.wikipedia.org/wiki/Digest_access_authentication
- OAuth: https://sv.wikipedia.org/wiki/OAuth