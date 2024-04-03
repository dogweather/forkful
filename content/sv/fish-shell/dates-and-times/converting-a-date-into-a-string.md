---
date: 2024-01-20 17:36:25.780034-07:00
description: 'Hur till: .'
lastmod: '2024-03-13T22:44:38.349858-06:00'
model: gpt-4-1106-preview
summary: .
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur till:
```
# Omvandla dagens datum till sträng
set -l today (date "+%Y-%m-%d")
echo $today
```
Output:
```
2023-04-14
```
```
# Omvandla specifikt datum till sträng med tid
set -l specific_date (date -d '2023-12-24 18:30' "+%Y-%m-%d %H:%M")
echo $specific_date
```
Output:
```
2023-12-24 18:30
```

## Fördjupning
I tidiga datasystem var hantering av datumformat begränsat och varierat stort mellan system. Moderna verktyg som Fish Shell använder inbyggda funktioner, `date` till exempel, för att hantera datum. Alternativt kan man använda externa verktyg som `strftime` i Fish för mer kontroll. När det gäller implementering, är det viktigt att tänka på tidszoner och lokalisering när datum omvandlas till strängar, så att formatet blir korrekt för avsedd publik.

## Se även
- Fish Shell dokumentation om datum och strängar: https://fishshell.com/docs/current/index.html#date
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- strftime dokumentation för fler formatmöjligheter: https://strftime.org/
