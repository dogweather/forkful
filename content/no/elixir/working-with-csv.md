---
title:                "Arbeide med csv"
html_title:           "Elixir: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

# Hvorfor
CSV (Comma-Separated Values) er et vanlig filformat for å lagre og utveksle data, spesielt i databaser og regneark. Det er nyttig for å organisere store mengder informasjon og kan være en effektiv måte å manipulere data på. Elixir har innebygde funksjoner for å håndtere CSV, noe som gjør det til et nyttig verktøy for alle som jobber med informasjonsbehandling eller dataanalyse.

# Hvordan
For å lese en CSV-fil i Elixir, kan du bruke funksjonen `File.stream!` og `CSV.decode`. Først må du åpne CSV-filen og lagre den i en variabel:

```elixir
file = File.stream!("data.csv") 
```

Deretter bruker du `CSV.decode` for å tolke filen som en CSV:

```elixir
CSV.decode(file, headers: true, trim: true) 
```

Her har vi satt `headers:` til `true` for å inkludere kolonnenavn, og `trim:` til `true` for å fjerne eventuelle mellomrom fra dataene. Resultatet vil være en liste med rader, hvor hver rad er en mappe med kolonnenavn som nøkler og tilhørende verdier.

# Dypdykk
Elixir har også forskjellige funksjoner for å håndtere og manipulere dataene i en CSV-fil. Du kan for eksempel filtrere rader basert på et kriterium, ved hjelp av funksjonen `Enum.filter`. Du kan også sortere rader etter en bestemt kolonne ved å bruke `Enum.sort_by`.

Elixir tilbyr også en ekstra `CSV.encode` funksjon for å skrive data til en CSV-fil. Du kan bruke `Enum.join` for å kombinere dataene i den ønskede CSV-formatet, og deretter bruke `File.write` for å skrive dataene til en ny fil.

# Se også
- [Offisiell Elixir dokumentasjon for CSV](https://hexdocs.pm/elixir/CSV.html)
- [Elixir School tutorial om CSV](https://elixirschool.com/lessons/specifics/csv/)
- [CSV bibliotek for Elixir](https://github.com/beatrichartz/csv)