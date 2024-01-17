---
title:                "Analysering av HTML"
html_title:           "Elixir: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-html.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Parsing av HTML er prosessen med å analyse og tolke HTML-kode for å kunne hente ut relevant informasjon fra en nettside. Dette gjøres ofte av utviklere for å kunne manipulere eller hente ut spesifikke data fra en nettside, for eksempel for å bygge webkrypere eller automatisere visse oppgaver.

Hvordan:
Vi kan bruke Elixir til å parse HTML ved å bruke biblioteker som for eksempel Floki eller Tesla. Her er et eksempel på hvordan du kan hente ut alle lenker fra en nettside:

```Elixir
html = Floki.parse(html_string)
links = Floki.find(html, "a")
for link <- links do
  href = Floki.attribute(link, "href")
  IO.puts(href)
end
```

Dette vil skrive ut alle lenkene som finnes i HTML-koden. 

Dypdykk:
Parsing av HTML har eksistert siden weben ble skapt på 90-tallet, og er fortsatt et vanlig verktøy for mange utviklere. Elixir har et stort utvalg av parsing-biblioteker, som gjør det enkelt å manipulere eller hente ut data fra nettsider. Alternativer til Elixir for parsing av HTML kan inkludere språk som Python eller Ruby. 

Se også:
- [ElixirFloki biblioteket](https://github.com/philss/floki)
- [ElixirTesla biblioteket](https://github.com/teamvest/tesla)