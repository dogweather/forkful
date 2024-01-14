---
title:    "Elixir: Store bokstaver i en streng"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Å kunne endre små og store bokstaver i en tekststreng er en vanlig prosess innen programmering. Det kan være for å lage en overskrift, formatere en liste eller bare for å gjøre teksten mer lesbar. I denne bloggposten vil vi fokusere på hvordan du kan gjøre dette i Elixir.

## Hvordan

For å kunne kapitalisere en tekststreng i Elixir, må du bruke funksjonen `String.capitalize/1` som tar inn en tekststreng som argument. La oss ta en titt på et eksempel:

```Elixir
iex> String.capitalize("hei, verden!")
"Hei, verden!"
```
Her ser vi at funksjonen har endret den første bokstaven i strengen til en stor bokstav.

Vi kan også bruke mer avanserte funksjoner som `String.upcase/1` og `String.downcase/1` for å gjøre hele tekststrengen til hhv. store eller små bokstaver.

```Elixir
iex> String.upcase("Dette er en tekststreng")
"DETTE ER EN TEKSTSTRENG"

iex> String.downcase("Dette er en tekststreng")
"dette er en tekststreng"
```

Det kan også være lurt å fjerne eventuelle hvite mellomrom før eller etter teksten ved å bruke funksjonen `String.trim/1`.

```Elixir
iex> String.trim("   Hei, verden!  ")
"Hei, verden!"
```

## Dykk dypere

Det er viktig å merke seg at disse funksjonene kun endrer første bokstav i teksten. Dersom du ønsker å endre første bokstav i hvert ord, kan du bruke `String.capitalize_every/1` eller `String.upcase_words/1` for å endre første bokstav til store bokstaver i hvert ord.

```Elixir
iex> String.capitalize_every("dette er første bokstav i hvert ord")
"Dette Er Første Bokstav I Hvert Ord"

iex> String.upcase_words("dette er store bokstaver i hvert ord")
"DETTE ER STORE BOKSTAVER I HVERT ORD"
```

Du kan også kombinere disse funksjonene med regex for å gjøre mer avanserte endringer i en tekststreng.

## Se også
- [Elixir Docs: String module](https://hexdocs.pm/elixir/String.html)
- [Elixir School: String basics](https://elixirschool.com/en/lessons/basics/string/)
- [Elixir Tutorial: Manipulating Strings](https://elixir-lang.org/getting-started/string.html)