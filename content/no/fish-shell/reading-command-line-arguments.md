---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Fish Shell: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesing av kommandolinje-argumenter er en vanlig oppgave for programvareutviklere. Det innebærer å hente informasjon som er gitt som argumenter når en kommando blir kjørt i terminalen. Dette gjør det mulig å lage programmer som kan ta imot spesifikke input fra brukeren og utføre bestemte oppgaver basert på dette.

Å lese kommandolinje-argumenter er en viktig del av å lage effektive og fleksible programmer, da det gjør det mulig for brukeren å gi spesifikke instruksjoner til programmet uten å måtte endre selve programmet.

## Slik gjør du det:

```Fish Shell``` har innebygde kommandoer for å lese kommandolinje-argumenter. For å få tilgang til argumentene, kan du bruke ```$argv``` variabelen som returnerer alle argumentene som en liste.

For å hente et spesifikt argument, kan du bruke ```$argv[index]``` hvor ```index``` er nummeret på argumentet du vil hente. For eksempel, hvis du vil hente det andre argumentet, kan du bruke ```$argv[2]```.

Her er et eksempel på hvordan du kan bruke denne funksjonen i et enkelt program:

```
#!/usr/bin/env fish

echo "Dette programmet ble kjørt med følgende argumenter: $argv"
echo "Det første argumentet er $argv[1] og det andre er $argv[2]"
```

Output:

```
$ ./program.sh hello world
Dette programmet ble kjørt med følgende argumenter: hello world
Det første argumentet er hello og det andre er world
```

## Dypdykk:

Kommandolinje-argumenter har vært en standardfunksjon for Unix-operativsystemer siden starten av 1970-tallet. Det finnes også alternative måter å lese argumenter på, som for eksempel å bruke en tredjeparts modul som ```argparse```. Det er også verdt å merke seg at antall argumenter og hvordan de håndteres kan variere avhengig av hvilket shell og operativsystem man bruker.

I ```Fish Shell``` blir alle kommandolinje-argumenter lagret som en liste i ```$argv``` variabelen. Du kan også bruke ```$argc``` variabelen for å få antall argumenter som ble gitt. I tillegg kan du bruke innebygde funksjoner som ```string split``` og ```set``` for å behandle argumentene på forskjellige måter.

## Se også:

- [The Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [The history of Unix shells](https://www.historicalinganalysis.com/en/the-history-of-unix-shells/)