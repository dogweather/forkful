---
title:    "Bash: Hämta nuvarande datumet"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumen är en viktig del av att skriva skript och automatisera uppgifter i Bash-programmering. Genom att kunna hämta det aktuella datumet kan du skapa dynamiska filnamn, logga data för övervakning eller helt enkelt hålla reda på tiden när skriptet kördes.

## Hur man gör

Det finns flera olika sätt att få det nuvarande datumet i Bash, vi kommer att titta på de två vanligaste metoderna.

Enkel sätt: För att få det nuvarande datumet i formatet "-MM-DD ÅÅÅÅ" kan vi använda "datum" -kommandot.

```Bash
datum +%Y-%m-%d
```

Output: 2021-06-22

Mer flexibel sätt: Det finns också möjlighet att ange ett specifikt datum- och tidsformat genom att använda "date" -kommandot och dess flaggor.

```Bash
datum +"%a, %b %d %Y" 
```

Output: tis, jun 22 2021

Det finns många olika flaggor du kan använda för att få den exakta datumformat du behöver. För att få en komplett lista över flaggor och format, kan du använda "man date" i terminalen.

## Djupdykning

Både "datum" och "date" -kommandon använder sig av standardinställningarna för region och tidszon för att avgöra det aktuella datumet. Detta kan vara användbart för att få en korrekt tid vid lokala eller globala användande, men om du vill ha ett specifikt datum och tidszon satt, kan du använda miljövariabler för att ändra det.

```Bash
TZ=America/New_York datum +%Y-%m-%d 
```

Output: 2021-06-21 (eftersom New York är fem timmar efter min nuvarande plats)

Detta kan också vara användbart om du vill få datumet och tiden för en specifik plats, oavsett var du befinner dig geografiskt.

## Se även

- [Datum- och tidsvariabler i Bash](https://www.linuxnix.com/linuxunix-shell-scripting-date-time-helpful-blueprint/)
- [Bash-guide för datum och tider](https://www.thegeekstuff.com/2013/11/bash-date-format/)
- [Man page för "datum"](https://linux.die.net/man/1/date)