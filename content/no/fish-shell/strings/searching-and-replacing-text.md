---
date: 2024-01-20 17:58:09.049121-07:00
description: "Hvordan: Erstatningen er gjort, og `dyr.txt` har n\xE5 'hund' der det\
  \ sto 'katt'."
lastmod: '2024-04-05T21:53:42.169839-06:00'
model: gpt-4-1106-preview
summary: "Erstatningen er gjort, og `dyr.txt` har n\xE5 'hund' der det sto 'katt'."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## Hvordan:
```Fish Shell
# Erstatt 'katt' med 'hund' i 'dyr.txt'
sed 's/katt/hund/' dyr.txt

# Se endringene umiddelbart med -i flagget
sed -i 's/katt/hund/' dyr.txt

# Erstatt alle instanser av 'katt' i filen
sed -i 's/katt/hund/g' dyr.txt
```
Erstatningen er gjort, og `dyr.txt` har nå 'hund' der det sto 'katt'.

## Dypdykk
Før Fish og moderne tekstbehandlere, var tekstmanipulasjon en tungvint affære. Tradisjonelle verktøy som `sed`, en strømredigerer, var revolusjonerende for sin tid og er fortsatt viktig i shell-skripting. Fish Shell gir en brukervennlig interface for disse operasjonene og er kompatibel med kjente Unix-kommandoer. Mens 'sed' handler om mønstersøk og tekstmanipulasjon på lavt nivå, kan Fish-scripting også ta i bruk kraftigere verktøy som 'awk' og 'grep' for mer komplekse oppgaver.

Alternativer til `sed` inkluderer tekstbehandlingsverktøy som 'awk', 'perl', og moderne programmeringsspråk som Python og Ruby. Visse Fish-funksjoner lar deg utføre enkle tekstmanipulasjoner uten å måtte kalle eksterne programmer, noe som kan forenkle skriptingen.

Detaljer å merke seg:
- Flagget 'g' i `sed` står for "global", som betyr at alle forekomster vil bli erstattet.
- `-i` flagget gjør endringene direkte i filen, så bruk med forsiktighet.
- Regulære uttrykk brukes for å spesifisere søkemønstre.

## Se Også
- Fish dokumentasjon for tekstmanipulasjon: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- GNU sed manual: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Regulære uttrykk-guide: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Historie og sammenligning av tekstbehandlere: [https://en.wikipedia.org/wiki/Comparison_of_text_editors](https://en.wikipedia.org/wiki/Comparison_of_text_editors)
