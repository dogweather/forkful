---
title:                "Fish Shell: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å koble sammen eller "konsolidere" tekststrenger er en viktig konsept innenfor programmering. Dette lar deg kombinere flere strenger til en, noe som kan være nyttig for å lage dynamiske utskrifter eller bygge komplekse kommandoer.

## Slik gjør du det

For å gjøre dette i Fish Shell, kan du bruke kommandoen `string join` etterfulgt av strengene du vil koble sammen. La oss si at vi vil koble sammen navnet og alderen til en person. Vi kan gjøre dette slik:

```
Fish Shell> set navn "Johan"
Fish Shell> set alder "32"
Fish Shell> string join $navn "er" $alder "år gammel"
```

Outputen vil være `Johan er 32 år gammel`. Her bruker vi `$` før variabelnavnet for å få verdien av variabelen, og "er" er den første strengen vår. 

Du kan også bruke `string split` for å bryte opp en streng basert på et gitt tegn. For eksempel:

```
Fish Shell> set mat "pølse,potet,salat"
Fish Shell> string split "," $mat
```

Outputen vil være en liste med hver matrett: `pølse potet salat`.

## Dypere dykk

I tillegg til de grunnleggende kommandoene `string join` og `string split`, kan Fish Shell også utføre mer avanserte handlinger når det gjelder strenger. Dette inkluderer å finne og erstatte deler av en streng, invertere en streng og konvertere den til store eller små bokstaver.

For å erstatte deler av en streng, kan du bruke `string replace` kommandoen. La oss si at vi har skrevet "Hello World" med en feil og ønsker å erstatte "World" med "Fish". Da kan vi bruke følgende kommando:

```
Fish Shell> string replace "Hello World" World Fish
```

Outputen vil være `Hello Fish`. Du kan også bruke `string toupper` og `string tolower` for å konvertere strenger til henholdsvis store eller små bokstaver.

## Se også

* [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
* [En guide til de mest brukte Fish Shell kommandoene](https://www.freecodecamp.org/news/fish-shell-tutorial/)
* [En liste med nyttige Fish Shell plugins](https://github.com/Fisherman/plugin-list/wiki)