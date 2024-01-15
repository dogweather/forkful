---
title:                "Store bokstaver i en streng"
html_title:           "Elixir: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne endre bokstavene i en streng til store bokstaver er en viktig ferdighet i programmering. Å kunne gjøre dette kan øke lesbarheten av koden din og hjelpe deg med å forstå datatypene du arbeider med. Det er også nyttig når man ønsker å sammenligne ulike strenger eller gjøre søk på deler av strenger.

## Hvordan

For å kapitalisere en streng i Elixir, kan du bruke funksjonen `String.upcase/1`. Her er et eksempel på hvordan du kan kapitalisere en streng:

```elixir
iex> String.upcase("hei")
"HEI"
```

Du kan også kapitalisere bare den første bokstaven i en streng ved å bruke funksjonen `String.capitalize/1`:

```elixir
iex> String.capitalize("god morgen")
"God morgen"
```

En annen mulighet er å bruke funksjonen `String.upcase_first/1` for å få store bokstaver på den første bokstaven i hver av ordene i en streng:

```elixir
iex> String.upcase_first("god morgen")
"God Morgen"
```

Når du skal arbeide med strenger, er det også nyttig å kunne sjekke om en streng er kapitalisert eller ikke. Du kan gjøre dette ved å bruke funksjonen `String.upcase?/1`:

```elixir
iex> String.upcase?("HEI")
true

iex> String.upcase?("Hei")
false
```

## Dypdykk

I Elixir, som i mange andre språk, er strenger representert som en liste av bokstaver. Det betyr at du kan bruke listeoperasjoner, som for eksempel `Enum.map/2`, for å utføre ulike operasjoner på en streng. Her er et eksempel på hvordan du kan kapitalisere en streng ved hjelp av `Enum.map/2`:

```elixir
iex> "hei" |> String.graphemes() |> Enum.map(&String.upcase/1) |> List.to_string()
"HEI"
```

I dette eksempelet bruker vi funksjonen `String.graphemes/1` for å konvertere en streng til en liste av bokstaver. Deretter bruker vi `Enum.map/2` for å kapitalisere hver bokstav, og til slutt konverterer vi tilbake til en streng ved hjelp av `List.to_string/1`.

Dette eksempelet viser bare en av mange måter å manipulere strenger på i Elixir. Resultatet er det samme som å bruke `String.upcase/1`, men det kan være nyttig å forstå hvordan strenger er representert for å kunne bruke dem mer effektivt i koden din.

## Se også

- Elixir dokumentasjon: ["String" module](https://hexdocs.pm/elixir/String.html)
- Elixir School: [Strings](https://elixirschool.com/en/lessons/basics/basics/#strings)