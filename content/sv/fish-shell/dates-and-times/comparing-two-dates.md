---
aliases:
- /sv/fish-shell/comparing-two-dates/
date: 2024-01-20 17:32:57.483532-07:00
description: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att fastst\xE4lla vilket som\
  \ \xE4r tidigast eller senast, eller om de \xE4r samma. Programmerare g\xF6r detta\
  \ f\xF6r att hantera\u2026"
lastmod: 2024-02-18 23:08:52.219655
model: gpt-4-1106-preview
summary: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att fastst\xE4lla vilket som \xE4\
  r tidigast eller senast, eller om de \xE4r samma. Programmerare g\xF6r detta f\xF6\
  r att hantera\u2026"
title: "J\xE4mf\xF6ra tv\xE5 datum"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att fastställa vilket som är tidigast eller senast, eller om de är samma. Programmerare gör detta för att hantera tidsbegränsningar, evenemang, eller tidslinjesfunktioner i applikationer.

## Hur man gör:
I Fish Shell kan du använda `date` för att jämföra datum. Ta en titt:

```Fish Shell
set date1 (date -d '2023-04-01' +%s)
set date2 (date -d '2023-04-15' +%s)

if test $date1 -lt $date2
  echo "Date1 är tidigare än Date2"
else if test $date1 -eq $date2
  echo "Date1 är samma som Date2"
else
  echo "Date1 är senare än Date2"
end
```

Exempel på utskrift:
```
Date1 är tidigare än Date2
```

## Djupdykning
Jämföra datum är som att jämföra siffror när datum omvandlas till Unix-tidsstämplar (sekunder sedan 1970-01-01). Historiskt sett var det mer komplicerat utan standardiserade funktioner. I Fish använder vi `date` med flaggan `+%s` för att få sekunder.

Alternativ inkluderar att använda externa verktyg som `datetime`-moduler i Python, eller SQL-funktioner i databaser. Fish är bra för enkla skript men kanske inte för komplexa tidsberäkningar.

Implementationsdetaljer att notera:
- Tidszoner hanteras inte här, vilket kan påverka jämförelsen.
- Prestanda är inte problematisk för små skript, men kan bli det för stora mängder datum.

## Se även
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils 'date'](https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation)
- [Unix Timestamp Conversion](https://www.unixtimestamp.com/)
- [Stack Overflow discussions on date comparisons](https://stackoverflow.com/search?q=compare+dates+fish+shell)
