---
title:                "Fish Shell: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan laste ned en nettside ved hjelp av kommandolinjeverktøyet Fish Shell? Les videre for å lære hvordan du kan enkelt laste ned informasjon fra nettsider med bare noen få linjer med kode!

## Hvordan

```Fish Shell
curl -o [filnavn] [nettside-URL]
```

Bruk denne enkle kommandoen for å laste ned en nettside og lagre den som en fil på din lokale datamaskin. Du kan også legge til flere muligheter, som for eksempel å laste ned kun headerinformasjon, eller å lagre filen i en bestemt mappe.

```Fish Shell
curl -I [nettside-URL]     # Laster ned kun headerinformasjon 
curl -o [mappe]/[filnavn] [nettside-URL]   # Lagrer filen i en bestemt mappe
```

## Dypdykk

Fish Shell bruker kommandoen "curl" for å laste ned nettsider. Dette er et populært verktøy brukt av utviklere for å få tak i informasjon fra ulike nettsider. Det støtter en rekke protokoller, som HTTP, FTP, og SFTP. Du kan også bruke ulike flagg for å få tilgang til spesifikke funksjoner.

For å laste ned en side fra en sikret HTTPS-tilkobling, kan du legge til "-k" flagget:

```Fish Shell
curl -k -o [filnavn] [HTTPS-nettsted-URL]
```

Det er også mulig å bruke Fish Shell for å laste ned nettsider periodisk, ved hjelp av kommandoen "crontab". Dette kan være nyttig for å holde seg oppdatert på informasjon fra ulike nettsider på en jevnlig basis.

## Se også

* [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
* [Curl nettsted](https://curl.se/)