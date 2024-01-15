---
title:                "Å jobbe med json"
html_title:           "Elixir: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å jobbe med JSON i Elixir? Som et programmeringsspråk som fokuserer på samspill mellom systemer og skalerbarhet, er det viktig å kunne arbeide med populære dataformater som JSON. JSON (Javascript Object Notation) er en vanlig måte å strukturere og lagre data på, og er mye brukt i webapplikasjoner og APIer.

## Hvordan

Det er enkelt å håndtere JSON i Elixir ved hjelp av noen få funksjoner og moduler. Først må du importere modulen `Jason`:

```Elixir
import Jason
```

For å konvertere en liste eller et kart til JSON-format, kan du bruke funksjonen `encode!/1`:

```Elixir
encode!([1, 2, 3])
# Output: "[1, 2, 3]"

encode!(%{name: "Elixir", version: 1.12})
# Output: "{\"name\":\"Elixir\",\"version\":1.12}"
```

For å konvertere JSON tilbake til en liste eller et kart, bruk funksjonen `decode!/1`:

```Elixir
decode!("[1, 2, 3]")
# Output: [1, 2, 3]

decode!("{\"name\":\"Elixir\",\"version\":1.12}")
# Output: %{name: "Elixir", version: 1.12}
```

Det er også mulig å håndtere feil ved å bruke funksjoner som `decode/1` og `decode!/2`.

For mer avansert arbeid med JSON, kan du også bruke biblioteket `jiffy` som tilbyr raske og effektive funksjoner for å lese og skrive JSON-data.

## Utforske videre

Å jobbe med JSON i Elixir handler ikke bare om å konvertere data til og fra JSON-format, men også å håndtere komplekse strukturer og behandle feil. Hvis du ønsker å utforske mer avansert bruk av JSON i Elixir, kan du ta en titt på følgende ressurser:

- [JSON Elixir Dokumentasjon](https://hexdocs.pm/elixir/Json.html)
- [jiffy - JSON-tilpasning for Elixir](https://github.com/davisp/jiffy)
- [Elixir forumtråd om JSON-håndtering](https://elixirforum.com/t/json-parsing-and-generation-with-elixir/4391)

## Se også

Her er noen nyttige lenker for å lære mer om Elixir og relaterte emner:

- [Elixir Offisiell Hjemmeside](https://elixir-lang.org/)
- [Elixir Offisiell Dokumentasjon](https://elixir-lang.org/docs.html)
- [Elixir Programmeringskurs på Udemy](https://www.udemy.com/course/the-complete-elixir-and-phoenix-bootcamp-and-tutorial/)