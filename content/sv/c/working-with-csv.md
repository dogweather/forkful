---
title:                "Arbeta med csv"
html_title:           "C: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV är en vanlig typ av filformat som används för att lagra och överföra data i tabellform. Det är en förkortning för "Comma-Separated Values" och fungerar som en mellanhand för att dela information mellan olika program och system. Programmörer använder CSV för att enkelt hantera och manipulera data som behöver organiseras i en tabellstruktur.

## Hur gör man:
Att arbeta med CSV i C är relativt enkelt. Det första steget är att inkludera biblioteket "stdio" som ger möjlighet att läsa och skriva till filer. Sedan är det viktigt att komma ihåg de grundläggande reglerna för CSV-formatet: varje fält är separerat med ett kommatecken och varje rad är separerad med ett radbrytningstecken.

Exempel på kod för att skriva data till en CSV-fil:

```C
#include <stdio.h>
int main() {
   //öppna filen för skrivning
   FILE *csvFile = fopen("data.csv", "w");

   //skriv data till filen
   fprintf(csvFile, "Hund,Katt,Kanin\n");
   fprintf(csvFile, "Brun,Svart,Vit\n");
   
   //stäng filen
   fclose(csvFile);
   
   return 0;
}
```

Output i filen 'data.csv':
```
Hund,Katt,Kanin
Brun,Svart,Vit
```

## Djupdykning:
CSV-formatet har funnits sedan tidigt 1970-tal och har blivit en standard för att överföra data mellan program och system på grund av dess enkelhet. En nackdel med CSV är att det inte kan hantera specialtecken som kommatecken eller radbrytningar inuti datafält, vilket kan orsaka problem vid hantering av mer komplex data.

Det finns alternativ till CSV som kan hantera specialtecken bättre, till exempel JSON och XML. Dock så blir dessa format mer komplicerade att hantera för både människor och datorer.

När det gäller implementation så är det viktigt att ha i åtanke att CSV-filer kan ha olika separatorer, inte bara kommatecken. Det finns också specifika regler för att hantera citattecken, som används för att inkludera specialtecken inuti datafält.

## Se även:
- [The history of CSV](https://www.computerworld.com/article/2280176/the-story-behind-the-developer-s-workhorse-csv-utilty.html)
- [Comparison of data interchange formats](https://en.wikipedia.org/wiki/Comparison_of_data_interchange_formats)
- [Reading and Writing CSV files in C](https://www.geeksforgeeks.org/csv-file-management-using-c/)