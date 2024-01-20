---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan är en metod för att kommunicera data mellan klienten (datorn som ber om information) och servern (datorn som lagrar informationen). Programmerare gör detta för att hämta, skicka eller manipulera data på webbservern.

## Hur gör man:

```Elixir
{:ok, response} = HTTPoison.get("http://httpbin.org/get")
IO.inspect(response.status_code)  # Visar statuskoden för HTTP-svaret, t.ex. 200
```
Denna kod skickar en GET-förfrågan till httpbin.org och visar statuskoden för svaret.

## Djupdykning

Historiskt sett är metoden för att skicka HTTP-förfrågningar ett grundläggande koncept som utvecklats från det tidiga internets dagar. Alternativ till HTTPoison inkluderar andra bibliotek som Hackney eller HTTPotion. Implementeringsdetaljer omfattar sättet att sätta ihop och sända begäran, såväl som tolkningen av det mottagna svaret.

## Se även:

För mer information om HTTP-begäran, HTTPoison och liknande ämnen, besök följande källor:
- Elixir officiella dokumentation: [https://hexdocs.pm/elixir/1.12/Kernel.html](https://hexdocs.pm/elixir/1.12/Kernel.html)
- HTTPoison GitHub Repo: [https://github.com/edgurgel/httpoison](https://github.com/edgurgel/httpoison)
- Hackney GitHub Repo: [https://github.com/benoitc/hackney](https://github.com/benoitc/hackney)