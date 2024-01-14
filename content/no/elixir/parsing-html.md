---
title:                "Elixir: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal man følge med på HTML-parsing? Hvis du jobber med webutvikling, er det en essensiell ferdighet å kunne trekke ut og bearbeide informasjon fra HTML-koden. Dette kan være å hente ut spesifikke data, utføre web scraping eller å lage skrapeverktøy for å automatisere prosesser.

## Slik gjør du det

For å få til parsing av HTML i Elixir, kan du bruke et bibliotek kalt Floki. La oss si vi ønsker å trekke ut alle lenker på en nettside. Vi starter med å installere biblioteket:

```Elixir
mix deps.get floki 
```
Deretter importerer vi biblioteket og bruker funksjonen `Floki.find/2` for å finne alle `a`-elementer på nettsiden:

```Elixir
import Floki

page = "https://www.eksempelside.no"
links = Floki.find(page, "a")
```

Dette vil gi oss en liste med alle lenkene på nettsiden. For å kun få ut lenkene som er synlige for brukeren, kan vi filtrere ved å bruke attributtet `:href`:

```Elixir
links = links |> Enum.filter(fn link -> link["href"] end)
```

Resultatet vil nå være en liste med de eksakte lenkene vi ønsker.

## Dykk dypere

HTML-parsing er en omfattende tema og Floki tilbyr mange andre funksjoner for å håndtere forskjellige tilfeller. Du kan for eksempel bruke `Floki.attribute/2` for å hente ut spesifikke attributter fra et element, eller `Floki.parse/1` for å konvertere hele HTML-dokumentet til en liste med datastrukturer som er lettere å jobbe med.

## Se også

- Offisiell Floki-dokumentasjon: https://hexdocs.pm/floki/
- Elixir School Tutorial for HTML-parsing: https://elixirschool.com/lessons/specifics/scrapping/