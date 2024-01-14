---
title:    "Bash: Stor bokstav i en streng"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

I Bash-programmering, er det ofte nødvendig å formatere en tekststreng på en spesifikk måte. En vanlig formateringsoppgave er å gjøre første bokstav i hvert ord til en stor bokstav, også kalt "kapitalisering". Dette kan være nyttig for å gjøre tekst for eksempel mer leselig eller for å følge bestemte konvensjoner. I denne bloggposten vil vi utforske hvordan man kan kapitalisere en tekststreng i Bash-programmering.

## Slik gjør du det

For å kunne kapitalisere en tekststreng i Bash, trenger vi å bruke en kombinasjon av Bash-programmering og Linux-verktøy. La oss si at vi har følgende tekststreng som vi ønsker å kapitalisere: "dette er en test". Vi kan bruke følgende kode for å gjøre dette:

``` bash
string="dette er en test"
echo "${string^}"
```

Dette vil gi følgende utdata: "Dette er en test". Vi bruker `${string^}` for å fortelle Bash at vi ønsker å kapitalisere strengen "string".

Vi kan også bruke en lignende syntaks for å kapitalisere kun den første bokstaven i strengen:

``` bash
string="dette er nok en test"
echo "${string^s}"
```

Dette vil gi følgende utdata: "Dette er nok en test". Her bruker vi `${string^s}` for å fortelle Bash at vi ønsker å kapitalisere den første bokstaven av "string".

## Dypdykk

Som nevnt tidligere, bruker vi en kombinasjon av Bash-programmering og Linux-verktøy for å kapitalisere en tekststreng. `${string^}` er et eksempel på Bashs parameterutvidelse, som er en funksjon som lar deg manipulere tekststrenger på forskjellige måter. I dette tilfellet bruker vi kapitaliseringen `^`.

En annen måte å kapitalisere en tekststreng på er å bruke Linux-verktøyet `tr`. Dette verktøyet lar oss gjøre ulike typer tekstmanipulasjoner, inkludert kapitalisering. Vi kan bruke følgende kode for å kapitalisere tekststrengen "dette er en test" ved hjelp av `tr`:

``` bash
echo "dette er en test" | tr '[:lower:]' '[:upper:]'
```

Dette vil gi følgende utdata: "DETTE ER EN TEST". Her bruker vi `tr` til å konvertere alle små bokstaver til store bokstaver.

Vi kan også kombinere både Bash-programmering og `tr` for å kapitalisere en tekststreng med en annen metode. For eksempel kan vi bruke følgende kode for å kapitalisere den første bokstaven i strengen "dette er nok en test":

``` bash
string="dette er nok en test"
echo "${string^} ${string:1}" | tr '[:lower:]' '[:upper:]'
```

Dette vil gi følgende utdata: "Dette Er Nok En Test". Her bruker vi `${string^}` for å kapitalisere den første bokstaven, og deretter `${string:1}` for å hente resten av strengen. Til slutt bruker vi `tr` for å konvertere alle små bokstaver til store bokstaver.

## Se også

- [Parameterutvidelse i Bash](https://www.linuxjournal.com/content/bash-parameter-expansion)
- [Bruke `tr` for tekstmanipulasjon](https://www.computerhope.com/unix/utrumper.htm)
- [Offisiell dokumentasjon for Bash](https://www.gnu.org/software/bash/manual/html_node/index.html)