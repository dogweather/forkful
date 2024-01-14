---
title:    "Elixir: Slette tegn som matcher et mønster"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi jobber med tekst, ønsker vi å fjerne visse tegn som matcher et spesifikt mønster. Dette kan være for å rense data eller for å gjøre tekstbehandlingen mer effektiv. I Elixir kan vi bruke en praktisk funksjon for å hjelpe oss med å gjøre dette.

## Hvordan gjøre det

For å slette tegn som matcher et mønster i Elixir, kan vi bruke funksjonen `String.replace/3`. Denne funksjonen tar tre argumenter: inputstrengen, mønsteret som skal matches og den nye teksten som skal erstatte det matchende mønsteret. La oss se på et eksempel:

```
Elixir
string = "Hei, jeg er en tekst"
String.replace(string, " ", "") # output kommer til å være "Heijegerentekst"
```

I dette eksempelet erstatter vi mellomromstegnene med en tom streng. Vi kan også bruke regex for å matche mer komplekse mønstre. For eksempel:

```
Elixir
string = "Jeg har en elixir-blogg"
String.replace(string, ~r/[a-z]{6,}/, "Elixir") # output kommer til å være "Elixir Elixir Elixir"
```

Her erstatter vi alle ord med seks eller flere bokstaver med "Elixir".

## Dypdykk

For å forstå hvordan `String.replace/3` fungerer, kan vi ta en nærmere titt på dens implementering. Den bruker en funksjon kalt `:re.replace/4` fra det underliggende biblioteket *re*. Den tar inn inputstrengen, regex-mønsteret og en funksjon som brukes til å erstatte matchende mønstre. Den bruker også en funksjon kalt `:unicode.replace/3` for å sikre at den støtter Unicode-tegn.

Vi kan også bruke `String.replace/3` for å behandle data før vi bruker den til å bygge en liste eller annen datastruktur. Dette kan hjelpe oss med å rengjøre og validere data før vi bruker den videre i applikasjonen vår.

## Se også

- Dokumentasjon for String.replace: https://hexdocs.pm/elixir/String.html#replace/3
- Dokumentasjon for re biblioteket: https://hexdocs.pm/elixir/Regex.html
- Dokumentasjon for unicode biblioteket: https://hexdocs.pm/elixir/Unicode.html