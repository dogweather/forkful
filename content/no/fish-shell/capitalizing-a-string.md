---
title:    "Fish Shell: Stort bokstaver en streng"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å endre en tekststreng slik at den begynner med stor bokstav? Dette kan være nyttig for å gjøre tekst mer lesbar, spesielt når den vises til brukere eller på skjermen. I denne bloggposten vil vi lære hvordan du kan gjøre dette ved hjelp av Fish Shell.

## Hvordan

```Fish Shell
set tekst "dette er en tekststreng"
echo $tekst | tr '[:lower:]' '[:upper:]'
```

Dette kodeeksempelet viser hvordan du kan bruke kommandoen "tr" til å konvertere alle små bokstaver i en tekststreng til store bokstaver. Ved å pipe resultatet til "echo" kommandoen, vil vi kunne se den endrede tekststrengen: "DETTE ER EN TEKSTSTRENG".

## Dypdykk

I Fish Shell finnes det også en innebygd funksjon for å endre en tekststreng til å begynne med stor bokstav. Denne funksjonen heter "string capitalize" og kan brukes på følgende måte:

```Fish Shell
set tekst "dette er en tekststreng"
echo (string capitalize $tekst)
```

I dette eksempelet bruker vi funksjonen for å først endre tekststrengen til å begynne med liten bokstav, og deretter tilbake til stor bokstav. Begge kodeeksemplene gir samme resultat: "Dette er en tekststreng".

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Forkapitalisering av tekststrenger i Bash](https://www.baeldung.com/linux/forkapitalisering-av-tekststrenger-bash)