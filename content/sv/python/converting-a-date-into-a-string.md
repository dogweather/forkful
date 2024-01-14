---
title:    "Python: Omvandla ett datum till en sträng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Varför

När du arbetar med datum i Python, kommer det ofta tillfälle då du behöver konvertera en datumvariabel till en sträng. Detta kan vara för att spara datumen i en textfil eller för att jämföra datum med andra värden. Vi kommer att utforska varför det är viktigt att kunna konvertera datum till strängar och hur man gör det på ett enkelt sätt.

## Hur man gör det

Det finns flera olika metoder för att konvertera en datumvariabel till en sträng i Python. En av de vanligaste är att använda strftime() -funktionen. Här är ett exempel på hur man gör det:

```Python
import datetime

today = datetime.datetime.today()
today_str = today.strftime("%d/%m/%Y")
print(today_str)
```

Detta kommer att ge dig dagens datum i formatet "dd/mm/yyyy". Du kan ändra formateringen efter dina behov genom att ändra på argumentet i strftime-funktionen. Till exempel kommer "%b %d, %Y" att ge dig datumet i formatet "Mån dd, yyyy".

## Djupdykning

När du konverterar en datumvariabel till en sträng, är det viktigt att du förstår formatet som du vill ha resultatet i. Det finns olika symboler som du kan använda för att ange olika delar av datumet, till exempel "%d" för dag, "%m" för månad och "%Y" för år. Du kan också inkludera text eller andra specialtecken i formatet, såsom "%B" för fullständigt månadsnamn eller "%a" för dagens förkortning. Genom att experimentera med olika symboler och kombinationer kan du anpassa resultatet efter dina behov.

## Se också

- [Python Datum och Tid Dokumentation](https://docs.python.org/3/library/datetime.html)
- [Enkel guide till formatering av datum i Python](https://www.programiz.com/python-programming/datetime/strftime)
- [Python för nybörjare: En introduktion till datum och tider](https://realpython.com/python-datetime/)