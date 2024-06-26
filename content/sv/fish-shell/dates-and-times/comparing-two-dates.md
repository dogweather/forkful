---
date: 2024-01-20 17:32:57.483532-07:00
description: "Hur man g\xF6r: I Fish Shell kan du anv\xE4nda `date` f\xF6r att j\xE4\
  mf\xF6ra datum. Ta en titt."
lastmod: '2024-03-13T22:44:38.350746-06:00'
model: gpt-4-1106-preview
summary: "I Fish Shell kan du anv\xE4nda `date` f\xF6r att j\xE4mf\xF6ra datum."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

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
