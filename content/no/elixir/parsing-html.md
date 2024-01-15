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

## Hvorfor

Hvorfor ville noen engasjere seg i å lese og analysere HTML? Vel, for å automatisere den kjedelige og tidkrevende oppgaven med å skrape informasjon fra nettsider. Ved å bruke Elixir til å parsere HTML kan du effektivt hente ut relevant data og deretter bruke det til å bygge applikasjoner eller automatiseringsskript.

## Slik gjør du det

For å parsere HTML i Elixir, kan du bruke biblioteket "Floki". Dette biblioteket lar oss analysere og utvinne data fra HTML ved hjelp av CSS-selektorer. La oss se på et eksempel:

```Elixir
# Installer biblioteket
mix deps.get floki

# Importer biblioteket og last inn HTML fra en nettside
iex> alias Floki
iex> html = Floki.parse_html("
      <div class='article'>
        <h1>Elixir er fantastisk!</h1>
        <p>Du kan bygge kraftige applikasjoner på kort tid med Elixir.</p>
      </div>")

# Bruk CSS-selektoren for å hente ut ønsket data
iex> Floki.find(html, "h1")
[{:open, "h1", [], []},
  "Elixir er fantastisk!",
 {:close, "h1"}]

iex> Floki.find(html, "p")
[{:open, "p", [], []},
  "Du kan bygge kraftige applikasjoner på kort tid med Elixir.",
 {:close, "p"}]
```

Som du kan se i eksempelet, returnerer "find" funksjonen dataen som et tuple. Første element er typen, deretter taggen og til slutt teksten som var inne i taggen. Dette gjør det enkelt å ekskludere eller manipulere dataen, avhengig av hva du skal bruke den til.

## Dypdykk

Floki gjør også det mulig å manipulere HTML ved hjelp av funksjonen "html_to_iodata". Dette konverterer HTML til en praktisk iodata-format, noe som gjør det enklere å jobbe med dataen. I tillegg til "find", har Floki flere andre funksjoner som lar deg hente ut data basert på spesifikke kriterier, som for eksempel "all_children" og "all_siblings".

## Se også

- [Floki dokumentasjon](https://hexdocs.pm/floki/)
- [Elixir offisiell hjemmeside](https://elixir-lang.org/)
- [Elixir for nybegynnere](https://elixir-lang.org/getting-started/introduction.html)