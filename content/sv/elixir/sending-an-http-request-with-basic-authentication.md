---
title:                ":Att skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: :Att skicka en http-begäran med grundläggande autentisering"
simple_title:         ":Att skicka en http-begäran med grundläggande autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skicka en HTTP-begäran med grundläggande autentisering är när en programmerare skickar en begäran till en server som kräver användaruppgifter för att bekräfta identiteten på den som begär en resurs. Programmerare använder detta för att säkra sina applikationer och skydda användaruppgifter.

## Hur man gör:
Här är ett exempel på hur man skickar en HTTP-begäran med grundläggande autentisering i Elixir:

```Elixir
req = HTTPoison.get("https://www.example.com", [], [basic_auth: {"username", "password"}])
```

Output:
```Elixir
{:ok, %HTTPoison.Response{...}}
```

## Djupdykning:
Att skicka HTTP-begäran med grundläggande autentisering har funnits sedan HTTP-protokollets början och är fortfarande ett av de vanligaste sätten att autentisera begäranden till servern. Alternativ till grundläggande autentisering inkluderar OAuth och JWT. När man skickar en begäran med grundläggande autentisering skickas användarnamn och lösenord krypterat i en Base64-kodad sträng tillsammans med begäran.

## Se även:
Läs mer om HTTPoison biblioteket här: https://hexdocs.pm/httpoison/HTTPoison.html