---
title:                "Slette tegn som matcher et mønster"
html_title:           "Elixir: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å slette tegn som matcher et mønster er en vanlig oppgave i programmering. Dette gjøres for å rydde opp i en tekststreng, fjerne uønskede tegn eller for å filtrere ut data som ikke er relevant for et bestemt formål.

Hvordan:

For å slette tegn som matcher et mønster i Elixir, kan du bruke funksjonen ```String.replace``` og angi mønsteret som skal matches som det første argumentet, og en tom streng som det andre argumentet. La oss si at vi har en streng med navnet "Elixir" og vi vil slette alle bokstavene "x" og "i" fra den. Vi kan gjøre det slik:

```Elixir

String.replace("Elixir", "xi", "")
```

Dette vil gi følgende output: "Elr". Vi sletter da tegnene "x" og "i" fra strengen.

Dypdykk:

I Elixir har vi flere alternativer for å slette tegn som matcher et mønster. En annen nyttig funksjon som vi kan bruke er ```String.replace_leading```, som lar deg slette tegn som matcher et mønster fra starten av en streng. Vi kan også bruke regulære uttrykk for å definere mønsteret vi ønsker å matche.

Det kan være nyttig å vite litt om historien bak dette konseptet. Sletting av tegn som matcher et mønster er en vanlig oppgave som har eksistert i programmering i lang tid. Elixir, som er et relativt nytt programmeringsspråk, gjør det enkelt å utføre denne oppgaven ved å tilby enkel syntaks og en rekke innebygde funksjoner.

Se også:

Hvis du vil lære mer om hvordan du arbeider med tekststrenger i Elixir, kan du se disse ressursene:

- [Offisiell Elixir dokumentasjon] (https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir koans] (https://elixirkoans.io/) - øvelsesoppgaver for å lære Elixir
- [Elixir Forum] (https://elixirforum.com/) - diskusjonsforum for Elixir-samfunnet