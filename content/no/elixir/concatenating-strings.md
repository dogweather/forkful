---
title:                "Sammenføying av strenger"
html_title:           "Elixir: Sammenføying av strenger"
simple_title:         "Sammenføying av strenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Concatenation, eller å sette sammen strenger, er en vanlig operasjon i programmering. Det betyr å kombinere to eller flere strenger til en enkelt streng. Programmører gjør dette for å bygge dynamiske strenger som kan brukes til å generere tekst på flyet, for eksempel å sette sammen en setning med variabel data.

## Hvordan gjøre det:
Det er flere måter å sette sammen strenger i Elixir, men den vanligste er å bruke ```<>``` operatøren. La oss se på et eksempel:

```
first_name = "John"
last_name = "Doe"

full_name = first_name <> " " <> last_name

IO.puts(full_name)

# Output:
# John Doe
```

I dette eksempelet har vi først definert to strenger, ```first_name``` og ```last_name```. Deretter brukte vi ```<>``` operatøren til å sette sammen de to strengene til en fullstendig navnestreng. Vi skrev deretter ut det endelige resultatet ved hjelp av ```IO.puts``` funksjonen.

## Dypdykk:
Concatenation er ikke en ny konsept innen programmering. Det har blitt brukt i mange år i forskjellige språk som Java og C++. Men i Elixir, kan du også bruke funksjonen ```String.concat``` til å sette sammen strenger. Dette er spesielt nyttig hvis du har en liste av strenger som du vil sette sammen til en enkelt streng.

Det er også viktig å nevne at Elixir er et funksjonelt programmeringsspråk, så de fleste av de vanlige måtene for å sette sammen strenger i andre språk, for eksempel med en ```+``` operatør, vil ikke fungere i Elixir. Derfor er det viktig å bruke ```<>``` operatøren eller ```String.concat``` funksjonen i stedet.

## Se også:
Hvis du vil lære mer om strenger og funksjonell programmering i Elixir, sjekk ut disse ressursene:

- [Elixir-docs: Strings](https://hexdocs.pm/elixir/String.html)
- [Learn X in Y Minutes: Elixir](https://learnxinyminutes.com/docs/elixir/)
- [The Elixir Programming Language Website](https://elixir-lang.org/)