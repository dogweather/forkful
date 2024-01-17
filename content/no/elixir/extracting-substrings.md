---
title:                "Utvinning av delstrenger"
html_title:           "Elixir: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Extracting substrings, eller å trekke ut delstrenger, er en måte å få tak i deler av en tekststreng på. Dette kan være nyttig når man ønsker å manipulere eller analysere bare en del av teksten. Programmere benytter seg av dette for å behandle data mer effektivt og få tilpasset informasjon for ulike formål.

# Hvordan:

Du kan bruke funksjonen `String.split` for å trekke ut delstrenger basert på en gitt separator, for eksempel mellomrom eller komma. Her er et eksempel på hvordan du kan bruke denne funksjonen:

Elixir
```
str = "Dette er en setning."
parts = String.split(str, " ")
IO.inspect(parts)
```
Output: `["Dette", "er", "en", "setning."]`

Du kan også eksplisitt spesifisere et antall deler du ønsker å få fra delstrengen. For eksempel, hvis du ønsker å få de to første ordene fra setningen ovenfor kan du bruke `String.split(str, " ", 2)` som vil gi følgende output:

Output: `["Dette", "er en setning."]`

# Dypdykk:

Å ekstrahere delstrenger er ikke et nytt konsept, det har vært brukt i ulike programmeringsspråk i lang tid. Dette er en grunnleggende funksjon som kan hjelpe programmerere å manipulere og prosessere tekst på en mer effektiv måte. Alternativer til `String.split` i Elixir inkluderer funksjoner som `String.slice` og `String.substr`.

I Elixir er tekststrenger representert som lister av tegn, noe som betyr at man kan manipulere dem ved å bruke vanlige listefunksjoner. Dette betyr at du også kan bruke `Enum`-modulen og dens funksjoner som `Enum.map` for å trekke ut delstrenger basert på bestemte kriterier.

# Se også:

- Elixir dokumentasjon for `String`- og `Enum`-modulene: https://hexdocs.pm/elixir/String.html og https://hexdocs.pm/elixir/Enum.html
- En tutorial om å arbeide med tekststrenger i Elixir: https://www.zohaib.me/working-with-strings-in-elixir/
- Diskusjon om forskjeller mellom `String.split` og `String.slice`: https://stackoverflow.com/a/32198895