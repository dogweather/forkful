---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka (parsing) HTML innebär att ta HTML-kod och omvandla det till datastrukturer i din programmeringsmiljö. Detta gör programmerare för att inse struktur, innehåll och layout direkt ur HTML-dokument.

## Så här gör du:
Vi använder den inbyggda `:eex` Motorn (Elixir Embedded) för att tolka HTML. Här är ett exempel:

```Elixir
:file.cd('/path/to/your/html')
{:ok, html} = File.read('index.html')
{:ok, tokens, _} = :eex.tokenize(html, 1, [])
tokens
```
Resultatet kommer att vara token-listan av HTML-dokumentet.

## Djupgående
När vi talar om historisk kontext kan HTML-parsing spåra sina rötter till de tidiga dagarna av webbutveckling där webbskrapa var ett populärt sätt att hämta data från webbsidor.

Det finns alternativ till `:eex` i Elixir för HTML-parsing, t.ex. Floki, en populär bibliotek för att söka HTML-dokument, inspirerad av Nokogiri.

Om vi kikar på detaljerna i implementationen arbetar `:eex`-motorn igenom HTML-strängen, hittar och tolkar HTML-taggar och skapar en lista av tokens.

## Se även
Vill du utforska mer? Kolla in följande länkar:

1. [Elixir :eex dokumentation](https://hexdocs.pm/eex/EEx.html)
2. [Floki library](https://github.com/philss/floki)
3. [Tolkning (parsing) på Wikipedia](https://en.wikipedia.org/wiki/Parsing)