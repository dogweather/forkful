---
title:    "Fish Shell: Å skrive en tekstfil"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive en tekstfil kan virke som en enkel oppgave, men det er faktisk en viktig del av programmering med Fish Shell. Ved å skrive en tekstfil kan du enkelt organisere og lagre informasjon som kan brukes i programmer eller som en del av en større prosess.

## Hvordan
Å skrive en tekstfil med Fish Shell er enkelt og kan gjøres ved hjelp av noen få kommandoer. La oss se på et eksempel der vi skal skrive ut noen velkomstmeldinger til en tekstfil:

```
fish << END | tee greeting.txt
    echo "Hei, hva heter du?"
    read name
    echo "Velkommen, $name!"
END
```

Etter å ha kjørt dette kodeblokk, vil du få opp et spørsmål om å skrive inn ditt navn. Deretter vil det bli laget en tekstfil med navnet "greeting.txt" som inneholder en personlig velkomstmelding. Du kan også legge til flere kommandoer etter "END" for å legge til flere linjer i tekstfilen.

## Dypdykk
Nå som du har lært hvordan du kan skrive en tekstfil med Fish Shell, kan du utforske flere muligheter for å bruke denne funksjonaliteten. For eksempel kan du bruke en tekstfil som et alternativ til å skrive inn flere kommandoer manuelt. Du kan også bruke tekstfiler til å lagre konfigurasjonsinnstillinger eller til og med skrive ut rapporter basert på data fra andre filer.

## Se også
- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Ulike måter å skrive tekstfiler på med Fish Shell](https://linuxize.com/post/how-to-write-data-to-a-file-in-fish/)