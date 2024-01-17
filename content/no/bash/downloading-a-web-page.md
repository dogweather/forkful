---
title:                "Å laste ned en nettside"
html_title:           "Bash: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Å laste ned" en nettside betyr å hente informasjon fra en nettside og lagre det på datamaskinen din. Programmere gjør dette for å analysere data, bruke det til å lage programmer eller for å bevare informasjonen for senere.

## Hvordan:
For å laste ned en nettside i Bash, bruk følgende kommando: 
```Bash
wget <nettside URL>
```
Dette vil laste ned nettsiden og lagre den med et generisk navn som "index.html" i din nåværende mappe. For å spesifisere et annet navn, legg til flagget "-O" etter kommandoen, etterfulgt av navnet du ønsker:
```Bash
wget <nettside URL> -O <navn>
```
Du kan også laste ned alle nettsidene som er lenket til på hovedsiden ved å bruke flagget "-r":
```Bash
wget -r <nettside URL>
```
Dette er nyttig for å få all informasjonen på en nettside og ikke bare hovedsiden. 

## Dypdykk:
Laste ned nettsider var en gang et komplisert prosess som involverte å lese og tolke HTML-kode. Men med dagens teknologi er det mye enklere å bruke verktøy som "wget" for å gjøre dette for deg. 
Alternativet til å bruke "wget" kan være å bruke et grafisk brukergrensesnitt for å laste ned nettsider, men dette kan ta lengre tid og være mindre presist.

## Se også:
For mer informasjon om "wget" og hvordan det fungerer, sjekk ut [den offisielle dokumentasjonen](https://www.gnu.org/software/wget/). Husk også at det finnes andre verktøy som kan brukes til å laste ned nettsider, som for eksempel "curl". Utforsk og finn det som fungerer best for deg og ditt behov.