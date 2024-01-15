---
title:                "Jämförande av två datum"
html_title:           "Bash: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart för att se om ett datum har passerat, om två datum ligger nära varandra i tid eller för att sortera datum i en lista. Detta kan vara särskilt användbart i datumhanteringssystem eller för att utföra tidsberäkningar.

## Hur man gör det

För att jämföra två datum i Bash kan du använda "date" kommandot tillsammans med "-d" flaggan. Du kan skriva "date -d '2020-01-01'” för att visa datumet 1 januari 2020, eller “date -d 'tomorrow + 1 month'” för att få datumet en månad framåt från imorgon.

```Bash
tag=`date +%d%m%Y`
if [ $tag  -ge 01032019 ]; then
echo "Datumet är efter 1 mars 2019"
else
echo "Datumet är före 1 mars 2019"
fi
```

I det här exemplet jämför vi ett datum skrivet i formatet "ddmmyyyy" med datumet 1 mars 2019 i samma format. Om datumet är efter 1 mars 2019 skrivs "Datumet är efter 1 mars 2019" ut, annars skrivs "Datumet är före 1 mars 2019" ut.

## Deep Dive

Vid jämförelse av datum är det viktigt att tänka på att datumet måste formateras på rätt sätt för att kunna jämföras korrekt. Om man till exempel jämför ett datum i formatet "yyyymmdd" med ett datum i formatet "ddmmyyyy" kommer jämförelsen inte att fungera som väntat. Det är också viktigt att ange tider vid jämförelser om man vill inkludera det i resultatet.

För att jämföra två datum i ett skript, är det också viktigt att ta hänsyn till variabeltyper. Om både variabelna är i formatet "mmmddyyyy" kan jämförelsen se annorlunda ut än om ena variabeln är i formatet "yyyy-mm-dd". Att förstå variabeltyper kan hjälpa till att undvika felaktiga datumjämförelser.

## Se även

- [Bash Guide - Date Commands](https://bash.cyberciti.biz/guide/Month,_weekday,_year_(_etc.)_in_simple_output_format)
- [Bash Reference Manual - Shell Arithmetic](https://tiswww.case.edu/php/chet/bash/bashref.html#Shell-Arithmetic)