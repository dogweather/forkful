---
title:    "Fish Shell: Att få den aktuella datumet"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumen är en viktig del av programmering i Fish Shell. Det gör det möjligt för oss att automatisera processer som kräver korrekt tidsstämplar, som loggning eller schemaläggning av uppgifter.

## Hur man gör

För att få den nuvarande datumen i Fish Shell, använder vi inbyggda funktionen `date`. Genom att skriva `date` i terminalen, kommer vi att få dagens datum och tid i standardformat.

```Fish Shell
date
```

Det är också möjligt att formatera datumen för att visa det i ett annat format. Till exempel kan vi använda flaggan `+%A` för att få veckodagen på svenska.

```Fish Shell
date +%A
```

Detta kommer att producera utmatningen "Torsdag" för idag, men om du vill få det på engelska istället kan du använda Y flaggan `B` tillsammans med `+%A`.

```Fish Shell
date +%AB
```

Detta kommer att producera utmatningen "Thursday". Det finns en hel del olika formateringsalternativ som du kan använda för att få den nuvarande datumen i önskat format, så se till att kolla på dokumentationen för Fish Shell för mer information.

## Djupdykning

Den inbyggda funktionen `date` är faktiskt en del av en familj av program som används för att hämta datum-och tidsinformation. Detta program, även känd som `coreutils`, är installerat på de flesta Linux-och Unix-baserade system. Det är också tillgängligt för MacOS som en del av Homebrew-paketet `coreutils`. Med detta i åtanke betyder det att du också kan använda `date` i andra skal, som Bash, om det är installerat på ditt system.

## Se även

- Fish Shell Dokumentation (https://fishshell.com/docs/current/)
- Coreutils Dokumentation (https://www.gnu.org/software/coreutils/manual/coreutils.html)
- Homebrew (https://brew.sh/)