---
title:                "Å laste ned en nettside"
html_title:           "PowerShell: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside handler om å hente informasjon fra en nettside og lagre den på enheten din. Programmerere gjør dette for å kunne analysere og manipulere data fra nettsiden, eller for å kunne automatisere bestemte oppgaver som å skanne nettsider for spesifikk informasjon.

## Slik gjør du det:

Å laste ned en nettside i PowerShell er enkelt og kan gjøres ved å følge disse trinnene:

1. Åpne PowerShell-konsollen.
2. Bruk kommandoen `Invoke-WebRequest` etterfulgt av nettsidens URL for å laste ned nettsiden. For eksempel: ```PowerShell Invoke-WebRequest https://www.example.com ```.
3. For å lagre nettsiden som en fil, kan du legge til `-OutFile` parameteren og gi filen et navn. Dette kan se slik ut: ```PowerShell Invoke-WebRequest https://www.example.com -OutFile example.html```.
4. Hvis du ønsker å utforske informasjonen som ble hentet fra nettsiden, kan du bruke kommandoen `Get-Content` etterfulgt av filnavnet. Dette vil gi deg innholdet i nettsiden i PowerShell-konsollen.

## Dykk dypere:

Historisk sett, har programmerere brukt kommandolinjeverktøy som `curl` eller `wget` for å laste ned nettsider, men i dag er PowerShell et kraftigere og mer fleksibelt alternativ. I tillegg gir `Invoke-WebRequest` -kommandoen mulighet for å hente informasjon fra nettsider ved hjelp av API-kall.

Et annet alternativ for å laste ned nettsider er å bruke et web scraping-verktøy som BeautifulSoup eller Scrapy. Disse verktøyene lar deg mer kontrollert hente informasjon fra nettsiden og kan være nyttige hvis du trenger å skrape informasjon fra flere nettsider.

Når du laster ned en nettside ved hjelp av PowerShell, blir informasjonen hentet som et `HTML`-dokument. Dette betyr at du kan bruke PowerShell til å manipulere og trekke ut spesifikk informasjon fra dokumentet.

## Se også:

- [PowerShell dokumentasjon for Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [BeautifulSoup dokumentasjon](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Scrapy dokumentasjon](https://docs.scrapy.org/en/latest/)