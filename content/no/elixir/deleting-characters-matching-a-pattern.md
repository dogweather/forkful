---
title:                "Elixir: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster i programmering er en nyttig ferdighet som kan hjelpe deg med å rydde opp i tekstbaserte data og effektivisere arbeidsflyten din. Dette kan være nyttig når du jobber med store tekstfiler eller behandler brukerinndata.

## Hvordan

Det er flere måter å slette tegn som matcher et mønster i Elixir. En enkel måte er å bruke `Regex.replace/3`-funksjonen, som tar inn tre argumenter: regex-mønsteret du vil matche, erstatningsstrengen og teksten du vil søke i. 

```
Elixir
Regex.replace(~r/hat/, "hatt", "Jeg har en blå hat.")
```

Dette vil gi følgende output:

```
"Jeg har en blå hatt."
```

Du kan også bruke regex-flagg for å gjøre søket ditt mer presist, for eksempel ved å ignorere store og små bokstaver. For å gjøre dette, bruk `:caseless`-flagget:

```
Elixir
Regex.replace(~r/hat/, "hatt", "Jeg har en blå Hat.", opts: [:caseless])
```

Dette vil også gi følgende output:

```
"Jeg har en blå hatt."
```

## Dypdykk

Når du sletter tegn som matcher et mønster, kan du også bruke regex-grupper til å erstatte deler av teksten din med variabler. For eksempel:

```
Elixir
Regex.replace(~r/([a-z])n([a-z])/, "\\1m\\2", "Jeg liker å spise bananer.")
```

Dette vil gjøre at bokstavene "n" og "e" byttes ut med "m" og "a", og du vil få følgende output:

```
"Jeg liker å spise bamanar."
```

Regex er en kraftig teknikk som kan brukes til å søke og manipulere tekst på mange forskjellige måter. Det er verdt å utforske alle de forskjellige funksjonene og regex-flaggene for å finne ut hva som fungerer best for deg og ditt prosjekt.

## Se også

- [Offisiell Elixir-dokumentasjon om Regex](https://hexdocs.pm/elixir/Regex.html)
- [Enkel introduksjon til regex i Elixir](https://www.erlang-solutions.com/blog/regexes-in-elixir-a-tutorial.html)
- [Elixir School sin guide til regex](https://elixirschool.com/lessons/basics/regex/)