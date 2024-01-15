---
title:                "Hämta aktuellt datum"
html_title:           "Bash: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

För många är det praktiskt att veta den nuvarande datum och tid. Det kan användas för att organisera scheman, planera evenemang eller helt enkelt hålla reda på tiden.

## Hur man gör

För att få den aktuella datumet i Bash använder man kommandot "date". Det finns olika format som kan användas för att anpassa output, men det vanligaste är ISO-formatet.

```Bash
date +%F
```

Detta kommer att returnera datumet i formatet ÅÅÅÅ-MM-DD, till exempel "2020-05-02". Om man vill ha en mer detaljerad output med både datum och tid, kan man använda kommandot "date" utan några extra flaggor.

```Bash
date
```

Det kommer att returnera tiden tillsammans med datumet, till exempel "Sat May  2 12:58:25 CEST 2020". Om man bara vill ha tiden, utan datumet, kan man använda flaggan "%T".

```Bash
date +%T
```

Det kommer att returnera tiden i formatet TT:MM:SS, till exempel "12:58:25". Mer information om olika flaggor och format finns i man-sidan för "date".

## Djupdykning

För att förstå hur "date"-kommandot fungerar är det bra att känna till Unix-tid. Det är antal sekunder som gått sedan 1 januari 1970, och används som en referenspunkt för att hålla reda på tiden i Unix-system. När "date"-kommandot används, hämtar det Unix-tiden och omvandlar den till den lokala tiden baserat på en fördefinierad tidszon. Det är en enkel men kraftfull funktion i Bash som är tillgänglig på alla Unix-baserade operativsystem.

## Se även

- [Bash-hemsidan](https://www.gnu.org/software/bash/)
- [Man-sidan för "date"](https://linux.die.net/man/1/date)
- [Unix-tid](https://sv.wikipedia.org/wiki/Unixtid)