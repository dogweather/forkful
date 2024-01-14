---
title:                "Fish Shell: Fra dato til streng"
simple_title:         "Fra dato til streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hva ville du gjort hvis du trengte å vise en dato i en lettleselig måte? Kanskje du ønsker å presentere den på en nettside eller i et dokument. Å konvertere en dato til en tekststreng er en enkel måte å gjøre dette på. Med Fish Shell får du tilgang til en rekke funksjoner for å konvertere datoer til ønsket format.

## Slik gjør du det

For å konvertere en dato til en streng i Fish Shell, kan du bruke `date` kommandoen. La oss si at vi vil konvertere dagens dato til en streng i formatet "DD.MM.YYYY". I terminalen skriver vi følgende kommando:

    ```Fish Shell
    set today (date +%d.%m.%Y)
    echo $today
    ```

Kommandoen `date +%d.%m.%Y` tar dagens dato og konverterer den til formatet etterspurt. Ved å bruke `set`, lagres resultatet i variabelen `today`. Deretter bruker vi `echo` for å skrive ut denne variabelen, som i dette tilfellet vil være dagens dato i ønsket format.

## Dypdykk

Fish Shell gir flere alternativer for å konvertere datoer til strenger. Du kan også spesifisere en annen dato enn dagens ved å bruke `date -d`, etterfulgt av datoen du vil konvertere til en streng. Her er noen eksempler:

- `date +%A` vil gi oss dagens navn
- `date +%B` vil gi oss månedens navn
- `date +%j` vil gi oss dagens nummer i året

Du kan også bruke `strftime` funksjonen for å tilpasse datoformatet enda mer. For mer informasjon om ulike formateringsalternativer, kan du sjekke ut man-siden for `date` kommandoen ved å skrive `man date` i terminalen.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [How-To Geek: How to Customize Date and Time Formats in the Linux Terminal](https://www.howtogeek.com/howto/17022/customize-date-and-time-formats-in-the-linux-terminal/)