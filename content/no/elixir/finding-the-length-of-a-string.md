---
title:    "Elixir: Å finne lengden av en streng"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Det å finne lengden på en streng er en vanlig operasjon i programmering. Det kan være nyttig for å vite hvor mange tegn som utgjør en tekst, eller for å sammenligne to strenger. I Elixir, som er et funksjonelt og dynamisk programmeringsspråk, er det flere måter å finne lengden på en streng på. I denne bloggposten vil vi utforske noen av disse metodene.

## Hvordan gjøre det

For å finne lengden på en streng i Elixir, kan vi bruke funksjonen `String.length/1`. Denne funksjonen tar inn en streng som argument og returnerer lengden på strengen som et heltall.

```elixir
String.length("Hei, verden!") #=> 13
```

Vi kan også bruke operatøren `<>` for å kombinere to strenger og deretter finne lengden på resultatet.

```elixir
"Hello" <> "World" |> String.length #=> 10
```

## Dypdykk

Det er viktig å merke seg at `String.length/1` funksjonen i Elixir faktisk ikke teller antall tegn, men antall bytes. Dette betyr at hvis vi har en streng med multibyte-tegn, vil funksjonen returnere antall bytes og ikke antall tegn. 

For å faktisk telle antall tegn i en streng, kan vi bruke `String.length/3` funksjonen. Denne funksjonen tar inn en streng, encoding og optional navn på språk som argumenter og returnerer antall tegn i strengen. Det er viktig å spesifisere encoding og språk for å få riktig antall tegn.

```elixir
String.length("こんにちは", "UTF-8", :ja) #=> 5
```

## Se også

- [Offisiell Elixir dokumentasjon om strenger](https://hexdocs.pm/elixir/String.html)
- [Elixir School tutorial om strenger](https://elixirschool.com/lessons/basics/strings/)