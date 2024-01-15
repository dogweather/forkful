---
title:                "A finne lengden av en streng"
html_title:           "Elixir: A finne lengden av en streng"
simple_title:         "A finne lengden av en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en viktig del av programmering. Det lar deg blant annet håndtere tekst data og lage mer dynamiske programmer.

## Hvordan

For å finne lengden til en streng, kan du bruke funksjonen `String.length()`. Den tar en streng som input og returnerer et heltall som representerer antall tegn i strengen.

```Elixir
iex> String.length("Hei, verden!")
12
```

Mer avanserte bruksområder kan inkludere å kombinere funksjonen med løkker for å finne lengden til en liste med strenger.

## Dypdykk

I Elixir, og funksjonelle programmeringsspråk generelt, er strenger immutable, det vil si at de ikke kan endres. Dette betyr at hver gang en streng blir manipulert, blir det egentlig laget en ny streng. Dette er viktig å huske når du bruker `String.length()` da den bare returnerer lengden av den gitte strengen, og ikke tar hensyn til eventuelle endringer som blir gjort på den originale strengen.

Å finne lengden av en streng kan også være nyttig når du skal håndtere input fra brukere, som for eksempel passord eller brukernavn. Ved å bruke `String.length()` kan du validere om inputen oppfyller kravene til lengde.

## Se også

- [Elixir Docs for String module](https://hexdocs.pm/elixir/String.html)
- [String manipulation in Elixir tutorial](https://elixirschool.com/en/lessons/basics/basics/#string-manipulation)