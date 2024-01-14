---
title:                "Haskell: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

# Varför

CSV-filer är ett vanligt sätt att lagra och dela data på, och att lära sig hur man arbetar med dem kan vara mycket fördelaktigt för programmerare. Det kan hjälpa till att organisera, manipulera och analysera data på ett effektivt sätt.

# Så här gör du

CSV (Comma-Separated Values) är en textbaserad filformat som används för att lagra tabulära data. Varje rad i filen representerar en rad i tabellen och varje kolumn separeras av ett kommatecken. Det finns många olika bibliotek för att arbeta med CSV i Haskell, men här är ett enkelt exempel på hur man läser in en CSV-fil och skriver ut dess innehåll på skärmen: 

```
import Text.CSV

-- Läs in CSV-filen med namnet "example.csv"
csv <- parseCSVFromFile "example.csv"

-- Skriv ut innehållet på skärmen
print csv
```

Detta kommer att producera följande output:

```
Right [["Name","Age","Occupation"],["Anna","30","Lawyer"],["Erik","42","Engineer"],["Lisa","25","Teacher"]]
```

Datatypen `Right` indikerar att läsningen var lyckad och att datat är strukturerat som en lista av listor. Varje lista representerar en rad i CSV-filen, och elementen i listan är strängar som motsvarar kolumnerna. Med hjälp av denna kunskap kan vi nu manipulera datat på önskat sätt.

En annan vanlig åtgärd är att skriva data till en CSV-fil. Detta kan uppnås med hjälp av funktionen `writeFile` från `Text.CSV` biblioteket. Detta är ett exempel på hur du kan skapa en ny CSV-fil och skriva in din data i den:

```
import Text.CSV

-- Skapa en lista med data
let data = [["Name", "Age", "Occupation"], ["Maria", "36", "Doctor"], ["Johan", "29", "Programmer"]]

-- Öppna en fil med namnet "new.csv" för att skriva till
handle <- openFile "new.csv" WriteMode

-- Skriv data till filen
writeFile handle $ printCSV data

-- Stäng filen
hClose handle
```

Detta skapar en ny CSV-fil med namnet "new.csv" innehållande data som du har skrivit in. Genom att förstå hur man läser och skriver till CSV-filer kan du lätt använda dem för att hantera stora mängder data inom din Haskell-kod.

# Djupdykning

För att verkligen bli bekväm med att arbeta med CSV-filer i Haskell finns det några andra viktiga aspekter att lära sig. Till exempel, om du arbetar med CSV-filer som innehåller numeriska värden, är det viktigt att korrekt hantera datatyper och konvertera data från strängar till numeriska värden. Genom att använda funktioner som `readMaybe` från `Text.Read` biblioteket kan du säkerställa att dina data är korrekt hanterade.

Det är också värt att nämna att CSV-filer ibland kan innehålla speciella tecken eller extra whitespace. Detta kan leda till problem under läsningen och behandlingen av data. Genom att använda funktioner som `trim` från `Data.CSV` biblioteket kan du enkelt ta bort dessa extra tecken och whitespace innan du behandlar data.

# Se även

- [Haskell Wiki - Working with CSV](https://wiki.haskell.org/Working_with_CSV) 
- [Hackage - Text.CSV](http://hackage.haskell.org/package/csv) 
- [Hackage - Data.CSV](http://hackage.haskell.org/package/Csv) 
- [Hackage - Text.Read](https://hackage.haskell.org/package/base/docs/Text-Read.html)