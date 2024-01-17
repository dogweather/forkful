---
title:                "Sända en http-begäran"
html_title:           "Elixir: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

HTTP Begäran med Elixir

## Vad & Varför?
Skicka en HTTP-begäran innebär att din dator begär information från en server på internet. Det är en viktig del av webbutveckling eftersom det låter dig hämta och kommunicera med andra webbsidor, program eller appar.

## Hur gör man?
För att skicka en HTTP-begäran i Elixir, kan du använda funktionen `HTTPoison.request/4` från HTTPoison-biblioteket. Du behöver bara ange vilken metod du vill använda (t.ex. `:get` eller `:post`), den fullständiga URL:en och eventuella ytterligare parametrar som krävs för begäran. Här är ett exempel på hur du skulle hämta information från Google:

```Elixir
response = HTTPoison.request(:get, "https://www.google.com")

IO.puts(response.body) # Skriver ut sidans HTML-innehåll till terminalen
```
I detta exemplet sparar vi begäran som en variabel `response` och sedan skriver vi ut innehållet i svarsobjektet med hjälp av `IO.puts`-funktionen.

## Djupdykning
HTTP (HyperText Transfer Protocol) är ett protokoll som används för att överföra data över internet. Det skapades ursprungligen för att låta webbsidor kommunicera med varandra, men används nu för att skicka olika typer av data och information över internet. Det finns olika sätt att skicka HTTP-begäran i Elixir, men HTTPoison är en populär och väldokumenterad lösning.

## Se även
- [Officiell Elixir Dokumentation för HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [En jämförelse av olika HTTP-klientbibliotek för Elixir](https://ohmybrew.com/posts/comparison-of-http-clients-for-elixir/)
- [En översikt av HTTP-protokollet](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)