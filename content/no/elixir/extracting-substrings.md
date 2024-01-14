---
title:                "Elixir: Utdrag av substringer"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger trenger vi bare en del av en tekststreng for å løse et problem eller utføre en oppgave. Dette er når vi trenger å ekstrahere substringer i Elixir.

## Hvordan
For å utvinne en del av en tekststreng i Elixir, bruker vi funksjonen `String.slice/3`. Denne funksjonen tar tre argumenter: tekststrengen, startindeksen og sluttpindeksen. La oss se på et eksempel:
```Elixir
name = "Alice"
String.slice(name, 0, 2)
```
Dette vil gi oss resultatet `"Al"` som er de første to tegnene i variabelen `name`. Vi kan også bruke negative indekser for å telle baklengs fra slutten av strengen. For eksempel, hvis vi vil ha de siste to bokstavene i `name`, kan vi bruke `-2` som sluttpindeksen:
```Elixir
String.slice(name, -2, -1)
```
Dette vil gi oss `"ce"` som er de to siste bokstavene i `name`.

Vi kan også bruke funksjonen `String.slice/2` for å ekstrahere en del av en tekststreng basert på indekser. Denne funksjonen vil ta inn tekststrengen og en liste av indekser. Her er et eksempel:
```Elixir
String.slice("banana", [0, 2, 4])
```
Dette vil gi oss `"bnn"` som er bokstavene på indeksene 0, 2 og 4 i strengen `"banana"`.

## Deep Dive
Vi har også mulighet til å bruke funksjonen `String.slice/2` med et tredje argument som en funksjon. Denne funksjonen vil ta hver bokstav i strengen og returnere `true` eller `false` basert på en gitt betingelse. Her er et eksempel hvor vi vil ekstrahere alle vokalene i en tekststreng:
```Elixir
String.slice("Elixir", fn char -> char in ~w(a e i o u A E I O U) end)
```
Dette vil gi oss `"Ei"` som er alle vokalene i strengen `"Elixir"`.

Husk at strenger i Elixir er en liste av tegn, så vi kan også bruke funksjonen `Enum.slice/2` for å ekstrahere substringer fra en liste. For eksempel, hvis vi har en liste som inneholder tallene `1` til `5`, kan vi bruke `Enum.slice/2` for å få de siste 3 tallene:
```Elixir
list = [1, 2, 3, 4, 5]
Enum.slice(list, -3, 2)
```
Dette vil gi oss `[3, 4]` som er tallene `3` og `4` i listen.

## Se også
- [Elixir dokumentasjon for `String.slice/3`](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir dokumentasjon for `Enum.slice/2`](https://hexdocs.pm/elixir/Enum.html#slice/2)