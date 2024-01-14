---
title:                "Bash: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför
CSV-formatet, även kallat komma-separerad data, är en vanligt använd datalagringsmetod för att organisera och spara tabelldata. Genom att lära sig hur man hanterar CSV-filer på ett effektivt sätt kan du underlätta din datahantering och skapa mer strukturerat och lättläst kod. 

## Hur man gör
Att arbeta med CSV-filer i Bash är relativt enkelt och kan spara mycket tid för programmerare. För att läsa in en CSV-fil kan du använda dig av kommandot `cat` tillsammans med rörledningar (`|`) och `awk`-kommandot för att läsa och hantera data. Exempelvis kan du skriva följande kodblock:

```Bash
cat test.csv | awk -F ',' '{printf "| %-15s | %-15s |\n", $1, $2}' 
```

Detta kommer att läsa in filen `test.csv` (som har första kolumnen som namn och andra kolumnen som ålder) och skriva ut datan i en tabell med vänsterjusterade kolumner, avskilda med rörledningar. Resultatet kan se ut så här:

```Bash
| Namn            | Ålder           |
| Emma            | 29              |
| Gustav          | 33              |
| Maria           | 27              |
| Anders          | 35              |
| Sofia           | 31              |
```

För att spara den bearbetade datan i en ny CSV-fil kan du använda `>` operatorn. Så en fullständig kommandorad kan se ut som följande:

```Bash
cat test.csv | awk -F ',' '{printf "| %-15s | %-15s |\n", $1, $2}' > formatad_data.csv 
```

Den nya filen `formatad_data.csv` kommer nu att innehålla den formaterade datan som vi såg ovan.

## Djupdykning
Det är viktigt att vara medveten om att CSV-filer kan skilja sig en del åt beroende på var de skapats. Detta kan innebära att vissa specialtecken inte hanteras på samma sätt i alla CSV-filer. Ett sätt att hantera detta är att använda sig av `tr`-kommandot för att hantera specialtecken innan du läser in filen med `awk`. 

En annan användbar funktion är att kunna sortera data från en CSV-fil baserat på en specifik kolumn. Detta kan göras med hjälp av `sort`-kommandot. Till exempel kan du sortera en CSV-fil efter namn med följande kommando:

```Bash
sort -t"," -k 1 test.csv 
```

Detta kommer att sortera filen `test.csv` baserat på första kolumnen, som antas vara namn (tack vare `-t` flaggan som speciferar separeringstecken och `-k` flaggan som specificerar vilken kolumn som ska användas för sorteringen).

## Se även
- AWK Command in Linux
- Bash Commands for Beginners
- Sorting Data Using the Linux Sort Command