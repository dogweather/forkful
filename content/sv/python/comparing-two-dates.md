---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Jämförelse av två datum handlar om att avgöra vilket av två datum som kommer först, eller om de är samma dag. Detta gör programmerare för att sortera händelser, räkna ut tidsskillnader, eller att utföra villkorliga operationer baserade på datum.

## Så här gör du:

Vi kan jämföra två datum i Python genom att använda datetime-modulen. Här är ett enkelt exempel:

```Python
from datetime import datetime

date1 = datetime(2022, 3, 1)
date2 = datetime(2022, 5, 20)

# Compare dates
if date1 < date2:
    print("date1 comes before date2.")
elif date1 == date2:
    print("date1 and date2 are the same day.")
else:
    print("date1 comes after date2.")
```

Exempelvis kommer utskriften blir "date1 comes before date2." eftersom 1 mars 2022 kommer före 20 maj 2022.

## Djupdykning

Historiskt sett, datumjämförelser i datorprogrammering har vanligtvis skött med hjälp av strängar eller tidsstämplade heltal. Men dessa metoder kan bli kladdiga och ineffektiva, så moderna högnivåspråk som Python har inbyggd funktionalitet för att hantera datum.

Alternativt kan vi använda datummetoden `date.toordinal()` att jämföra datum. Denna metod returnerar ett heltal representerande hur många dagar har gått sedan basdagen (1 januari 1). Python stödjer också operatoröverlagring, vilket innebär att vi kan använda vanliga jämförelseoperatorer (som <, >, och ==) att jämföra datetime-objekt.

Detaljer att notera inkluderar att Python jämför datum objekt ned till mikrosekunden. Också, tidszonsmedvetna datum kan orsaka oväntade resultat om de inte hanteras korrekt.

## Se även

För mer information om Python-datumobjekt och jämförelser, se följande länkar: 

- Python's inbyggd datetime-modul: https://docs.python.org/3/library/datetime.html
- Stack Overflow diskussion om datumjämförelser: https://stackoverflow.com/questions/32723150/how-to-compare-two-dates-in-python
- Dive Into Python: Arbeta med datum och tid: http://diveintopython3.problemsolving.io/dates-and-times.html