---
title:                "Elixir: Slette tegn som matcher et mønster"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger må vi gjøre endringer i våre Elixir-programmer for å forbedre ytelsen eller forenkle koden. En måte å gjøre dette på er å slette tegn som matcher et visst mønster. Dette kan bidra til å rydde opp i koden og gjøre den mer lesbar.

## Hvordan

Å slette tegn som matcher et mønster kan gjøres ved hjelp av Elixir's `String.replace/3`-funksjon. La oss si at vi har en streng som inneholder tall og vi ønsker å slette alle tall fra 0 til 5.

```Elixir
input = "Jeg har 3 hester, 5 katter og 2 hunder"
String.replace(input, ~r/[0-5]/, "") # "Jeg har  ,   katter og  hunder"
```

Vi bruker et regulært uttrykk for å matche alle tall fra 0 til 5 og bytter dem ut med en tom streng. Dette gjør at alle tall blir slettet fra originalstrengen.

Vi kan også bruke `[...]` for å spesifisere hvilke tegn vi vil slette. La oss slette alle tegn i et alfabet fra a til c:

```Elixir
input = "Jeg elsker Elixir!"
String.replace(input, ~r/[a-c]/, "") # "Jeg elsker lixir!"
```

På denne måten kan vi enkelt slette enkelttegn, tall eller symboler som passer til vårt definerte mønster.

## Dypdykk

Når vi bruker regulære uttrykk for å matche et mønster, kan vi også bruke spesielle symboler for å utvide søket vårt. For eksempel kan vi bruke `.` for å matche ethvert tegn, `+` for å matche en eller flere forekomster av et tegn og `*` for å matche null eller flere forekomster. Dette gjør det mulig å lage mer komplekse søkemønstre.

Vi kan også bruke `^` og `$` for å matche begynnelsen og slutten av en streng. Dette kan være nyttig når vi vil slette noe som ligger i starten eller slutten av en streng.

## Se også

- [Elixir string manipulation](https://elixir-lang.org/getting-started/basic-types.html#string-manipulation)
- [Regular expressions in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Elixir string module](https://hexdocs.pm/elixir/String.html)