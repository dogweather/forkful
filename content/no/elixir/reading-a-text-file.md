---
title:    "Elixir: Lese en tekstfil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese og behandle tekstfiler er en nødvendighet for de fleste programmerere. Enten du jobber med store datafiler eller bare trenger å analysere logger, vil du sannsynligvis måtte lese en tekstfil på et tidspunkt. I denne bloggposten vil vi utforske hvordan du kan lese en tekstfil ved hjelp av Elixir-programmeringsspråket.

# Hvordan lese en tekstfil med Elixir

Elixir har et innebygd bibliotek kalt `File` som lar oss lese og skrive til filer. For å lese en tekstfil, må vi først åpne filen ved hjelp av `File.open/2`-funksjonen. Denne funksjonen tar to argumenter - filbanen og modusen. Modusen brukes til å avgjøre om filen skal åpnes for lesing eller skriving. I vårt tilfelle vil vi bruke modusen `:read` for å åpne filen for lesing.

For å lese innholdet i filen, bruker vi `IO.read/2`-funksjonen og passerer inn filhandelen som et argument. Dette vil returnere innholdet i filen som en streng. Hvis vi vil ha filinnholdet linje for linje, kan vi bruke `IO.stream/2`-funksjonen og deretter `Stream.each/2`-funksjonen for å skrive ut hver linje i filen.

Her er et eksempel på hvordan vi kan lese en tekstfil med Elixir:

```elixir
# Åpne filen for lesing
{:ok, file} = File.open("min_fil.txt", [:read])

# Les innholdet i filen som streng
innhold = IO.read(file)

# Skriv ut filinnholdet linje for linje
IO.stream(file) |> Stream.each(&IO.puts(&1))
```

Output:
```
Dette er en testtekst
som jeg har lagt til i filen.
Du kan også bruke `line`-funksjonen
for å lese en linje av gangen.

# Dypdykk

I tillegg til å lese filer på tradisjonell måte, har Elixir også et kraftig bibliotek kalt `File.Stream` som lar deg behandle filer som en strøm av data. Dette gjør det enkelt å behandle store datafiler uten å måtte lese hele filen inn i minnet.

For å bruke dette biblioteket, må vi først åpne filen som en strøm ved hjelp av `File.Stream.open/2`-funksjonen. Deretter kan vi bruke funksjoner som `File.Stream.unfold/3` og `File.Stream.map/2` for å behandle dataene i filen. Dette kan være nyttig for å filtrere, transformere eller aggregere data på en effektiv måte.

# Se også

- [Elixir File Module](https://elixir-lang.org/getting-started/basic-types.html#file-module)
- [Elixir File.Stream Module](https://hexdocs.pm/elixir/File.Stream.html)