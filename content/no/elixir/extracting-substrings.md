---
title:                "Uttrekking av delstrenger"
html_title:           "Elixir: Uttrekking av delstrenger"
simple_title:         "Uttrekking av delstrenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du jobber med tekstbehandling eller bearbeiding av data, er det sannsynlig at du på et eller annet tidspunkt vil trenge å trekke ut deler av en tekststreng. Å kunne ekstrahere substrings kan hjelpe deg med å manipulere data mer effektivt og gjøre komplekse oppgaver enklere å håndtere.

## Hvordan gjøre det
Å ekstrahere substrings i Elixir er enkelt og intuitivt. Du kan bruke funksjonen `String.slice/2` og øke substringsens posisjon og lengde. Enkelt sagt, du gir `String.slice/2` to argumenter -- en tekststreng og et utvalgspunkt, og funksjonen vil returnere en ny streng som inneholder en del av den opprinnelige strengen basert på utvalgsparameterene du ga. La oss se et eksempel:

```Elixir
original_streng = "Hei, verden!"
substring = String.slice(original_streng, 3, 5)
IO.puts(substing)
```

Dette vil returnere "i, ve", som er substringsen som starter på posisjon 3 og har en lengde på 5 tegn.

Du kan også bruke utfall fra andre funksjoner som inndata for `String.slice/2`. For eksempel, hvis du bruker `String.split/2` for å dele en streng opp i en liste av substrings, kan du bruke en av disse substringsene som inndata for `String.slice/2` for å få en del av den opprinnelige strengen.

## Dypere dykk
Elixir har også andre funksjoner for å ekstrahere substrings som `String.substring/2` og `String.slice/3`. Disse lar deg sette en startposisjon og sluttposisjon for substringsen du vil ekstrahere. Du kan også bruke regulære uttrykk sammen med `Regex.run/3` og `Regex.scan/3` for å filtrere og ekstrahere spesifikke substrings fra en tekststreng.

## Se også
- [Elixir dokumentasjon for String-modulen](https://hexdocs.pm/elixir/String.html)
- [Nettstedet for Elixir programmeringsspråket](https://elixir-lang.org/)