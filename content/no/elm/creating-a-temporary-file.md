---
title:                "Opprette en midlertidig fil"
html_title:           "Elm: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Oppretting av en midlertidig fil er en vanlig praksis blant utviklere for å lagre midlertidig data som er nødvendig i løpet av programkjøring. Dette kan være data som bare er nødvendig for en kort periode, og som ikke skal lagres permanent i systemet.

## Hvordan:
Å opprette en midlertidig fil i Elm er enkelt og kan gjøres ved å bruke funksjonen `File.temporary`. Her er et eksempel på å opprette en midlertidig fil med navnet "temp.txt" og skrive teksten "Dette er en midlertidig fil" til den:

```Elm
tempFil = File.temporary "temp.txt"
       |> Task.andThen (\res -> File.write tempFil "Dette er en midlertidig fil")
```

## Dykk dypere:
Opprettelsen av midlertidige filer har vært en etablert praksis i programmering i lang tid. I eldre programmeringsspråk, som C og C++, var det vanlig å bruke midlertidige filer for å lagre data mellom steg i en prosess. I dag er det også flere alternativer til å opprette midlertidige filer, som å bruke i-minnet databaser eller direkte arbeide med data i minnet.

Når man oppretter en midlertidig fil, vil det bli opprettet en unik fil i systemet som kun er tilgjengelig under programkjøringen. Når programmet avsluttes, vil denne filen automatisk bli slettet.

## Se også:
- [Offisiell dokumentasjon for File-modulen i Elm](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Enkel og kortfattet forklaring av hva en midlertidig fil er](https://www.linode.com/docs/tools-reference/linux-filesystem-structure/#temporary-files)