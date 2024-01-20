---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML er prosessen med å dekode HTML-språkets syntaks for å forstå og manipulere strukturen på en webside. Programmerere gjør dette for å hente, analysere eller endre data på nettsteder mer effektivt.

## Hvordan gjøre det:

La oss prøve å bruke `Floki`, et populært Elixir bibliotek for HTML-parsing.

```Elixir
# Først installerer vi Floki.
defp deps do
  [{:floki, "~> 0.31"}]
end

# Importer Floki i koden din.
import Floki

# Si at vi aspirerer for å få tittelen på en nett side.
html = "<html><head><title>Elixir - The power of simplicity</title></head></html>"

# Vi kan få tittelen ved bruk av `find` funksjonen.
title = html |> Floki.find("title") |> Floki.raw_html

# Nå, 'tittel' variabelen inneholder 'Elixir - The power of simplicity'.
IO.inspect(title)
```

## Dypdykk:

HTML parsing har vært en nødvendig del av programmering siden webens begynnelse. I Elixir bruker vi biblioteker som `Floki`.

Alternativt kan man bruke `Meeseeks` eller `Mochiweb`. Valget avhenger av dine spesifikke krav og preferanser.

Når det kommer til implementeringsdetaljer i `Floki`, bruker den Elixir's kraft til å gjøre parsing raskt og effektivt. Den bruker `:mochiweb_html` på bunnen for å parse HTML og deretter bruker sin egen algoritme for å gjøre trærne søkbare.

## Se også:

- [Floki Dokumentasjon](https://hexdocs.pm/floki)
- [Alternativer til Floki](https://elixirforum.com/t/how-do-you-parse-html-in-elixir/2385)