---
title:                "Å bruke regulære uttrykk"
html_title:           "Elixir: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bør du lære om regulære uttrykk? Fordi de er et kraftig verktøy for å manipulere tekst på en effektiv og presis måte i Elixir programmering. De kan hjelpe deg med å gjøre komplekse søk og erstattingsoperasjoner, noe som sparer deg for mye tid og krefter i programmering.

## Hvordan

For å bruke regulære uttrykk i Elixir, må du først importere Regex-modulen. Dette gjøres ved å skrive `import Regex` i begynnelsen av filen din. Deretter kan du opprette et Regex-objekt ved å bruke `~r` og plassere uttrykket ditt mellom to skråstreker. For eksempel, for å finne alle forekomster av ordet "heis" i en streng, kan du bruke `~r/heis/`.

```Elixir
import Regex

str = "Denne heisen går opp og ned"
regex = ~r/heis/
Regex.scan(regex, str) |> Enum.to_list #=> ["heis", "heis"]
```

For å erstatte et ord med et annet, kan du bruke `Regex.replace/3` funksjonen og angi hvilken del av strengen som skal erstattes og hva den skal erstattes med.

```Elixir
import Regex

str = "Jeg har en grønn bil"
regex = ~r/grønn/
Regex.replace(regex, str, "rød") #=> "Jeg har en rød bil"
```

## Dypdykk

Regulære uttrykk kan være forvirrende i begynnelsen, men det er verdt å ta seg tid til å lære dem. En ting å merke seg er at de er case-sensitive, så `~r/grønn/` vil ikke matche `grønn` eller `Grønn`. I tillegg kan du bruke karakterklasser for å matche et hvilket som helst antall tegn, for eksempel `~r/grø[ae]nn/` vil matche både `grønn` og `grånn`. Du kan også bruke kvantareller som `*` og `+` for å matche for eksempel `grønn` eller `grøøøønn`. For å lære mer om regulære uttrykk og hvordan de fungerer, kan du se på dokumentasjonen for Regex-modulen eller prøve å øve på koding med dem.

## Se også

- [Elixir Regex-dokumentasjon](https://hexdocs.pm/elixir/Regex.html)
- [Elixir Regex Playground](https://elixirplayground.sh/)
- [Elixir regular expression cheatsheet](https://millermedeiros.github.io/mdoc/examples/elixir_by_example/07-strings_regex.html)