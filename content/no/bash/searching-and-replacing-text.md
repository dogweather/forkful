---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søk og erstatt funksjonen er en aktivitet der vi finner en spesifikk tekstlinje (eller mønster) i en tekstfil for så å bytte den ut med noe annet. Programme har bruk for denne funksjonen fordi det gjør det mulig å utføre store endringer på kode eller data uten å måtte gjøre det manuelt.

## Hvordan:

Vi kan bruke 'sed' kommandoen for å søke og erstatte tekst i en fil ved hjelp av følgende syntaks:

```Bash
sed 's/jakt/bys'/ filnavn
```

Her erstatter vi ordet 'jakt' med 'bys' i den gitte filen. Hvis du vil lagre endringene tilbake i den opprinnelige filen, bruk '-i' alternativet.

```Bash
sed -i 's/jakt/bys'/ filnavn
```

Hvis det er flere forekomster av 'jakt' på en linje og du vil erstatte dem alle, legger du til 'g' etter 's/jakt/bys'.

```Bash
sed 's/jakt/bys/g' filnavn
```

## Dykk Ned:

Bash ble utgitt i 1989 av Brian Fox, som en gratis erstatning for Bourne Shell. 'sed' kommandoen (kort for 'stream editor') som brukes i søk og erstatt, har faktisk vært rundt siden midten av 1970-tallet, og er en del av UNIX-familien.

Alternativer til 'sed' inkluderer 'awk', en annen tekstbehandlingsverktøy, og Perl, et fullverdig programmeringsspråk som er godt kjent for sine tekstbehandlingsferdigheter.

Det er viktig å merke seg at 'sed' ikke faktisk endrer filen, men heller lager en ny tekststrøm som sendes til standard utdata. Hvis '-i' alternativet er brukt, vil originalfilen bli erstattet av denne nye teksten.

## Se Også:

1. GNU 'sed' Manual: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
2. 'awk' Manual: [https://www.gnu.org/software/gawk/manual/](https://www.gnu.org/software/gawk/manual/)
3. Perl Dokumentasjon: [https://www.perl.org](https://www.perl.org)