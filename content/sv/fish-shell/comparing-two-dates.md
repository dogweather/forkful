---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och varför?
Jämföra datum handlar om att se vilket datum som kommer före det andra, eller om de är samma dag. Programmerare gör detta för att kunna hantera tidsbaserade händelser, som bokningar och uppgiftspåminnelser.

## Så här gör du:
Fish Shell erbjuder flera mekanismer för att jämföra datum. Vi kan använda `date` kommandot för att få antalet sekunder sedan ”unix epoch” (1970-01-01 00:00:00 UTC) och sedan jämföra dessa tal.

```Fish Shell
# Sätt dagens datum
set today (date "+%s")

# Sätt ett datum i framtiden
set future_date (date -d '2022-12-12' "+%s")

# Jämför datumen
if test $today -lt $future_date
   echo "Idag är tidigare än framtida datum"
else if test $today -eq $future_date
   echo "Det är idag!"
else
   echo "Idag är senare än framtida datum"
end
```

När du kör detta script får du ett uttalande om de relativa positionerna för dagens datum och det framtida datum du ställde in.

## Fördjupning
Unix "epochtid" är en global standard för att mäta tidsintervall, som började den 1 januari 1970. Det är ett universellt sätt att jämföra två ögonblick i tiden, oavsett tidsskillnader eller tidszoner.

Det finns också andra sätt att jämföra datum på, till exempel att omvandla datum till strängar och jämföra dem lexicografiskt. Men detta kan ge problem med olika datumformat och tidszoner.

Det viktiga när man jämför datum i Fish Shell är att omvandla datumen till samma format. Denna metod omvandlar dem till antalet sekunder sedan Unix-epoken, vilket ger en direkt jämförbar siffra oavsett ursprungligt datumformat.

## Se även
För mer information om datum och tid i Fish Shell, se den officiella Fish Shell dokumentationen: https://fishshell.com/docs/current/commands.html#date

För mer information om Unix Epoch, kontrollera denna länk: https://en.wikipedia.org/wiki/Unix_time