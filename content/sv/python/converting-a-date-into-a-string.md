---
title:    "Python: Omvandla ett datum till en sträng"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera ett datum till en sträng är ett vanligt behov när man programmerar i Python. Det kan till exempel vara för att visa ett datum i ett läsbart format eller för att använda det som en del av en filnamnsgenerering.

## Hur man gör det

För att konvertera ett datum till en sträng i Python använder man funktionen `strftime()`. Detta är en inbyggd funktion i språket som tillåter dig att formatera ett datum baserat på ett specifikt format.

Ett enkelt exempel är att skapa en sträng som representerar dagens datum i det vanliga formatet "yyyy-mm-dd":

```Python
from datetime import date
today = date.today()
today_str = today.strftime("%Y-%m-%d")
print(today_str)
```

Detta kommer att producera följande output:

```
2021-01-01
```

Som du kan se använde vi funktionen `strftime()` för att formatera dagens datum till önskat format "%Y-%m-%d", där "%Y" står för årtal, "%m" för månad och "%d" för dag.

Det finns många olika formatteringsalternativ som kan användas med `strftime()`. Till exempel, om vi vill ha dagens datum i ett mer läsbart format, som "1 januari 2021", kan vi använda följande kod:

```Python
from datetime import date
today = date.today()
today_str = today.strftime("%-d %B %Y")
print(today_str)
```

Output blir då:

```
1 januari 2021
```

Som du kan se, använde vi här formatteringarna "%-d" för att få ett enställigt datum utan nolla framför, "%B" för att få månadens fullständiga namn och "%Y" för att få fyrsiffrigt årtal.

## Djupdykning

För att kunna använda `strftime()` måste du först skapa ett `datetime`-objekt med ett giltigt datum. Detta kan du göra genom att använda `date()`, `datetime()` eller `strptime()` funktionerna beroende på hur dina datum är formaterade.

När du väl har ditt `datetime`-objekt kan du använda `strftime()` för att formatera datumet till en sträng enligt dina behov. Tänk på att använda korrekta formatteringar för att få rätt resultat, annars kan det resultera i felaktiga strängar.

## Se också

* [Strftime och Strptime Referens](https://strftime.org/)
* [Hur man hanterar datum och tid i Python](https://realpython.com/python-datetime/)

<b>______________________________________________________________________________</b>

### Se också

* [Strftime och Strptime Referens](https://strftime.org/)
* [Hur man hanterar datum och tid i Python](https://realpython.com/python-datetime/)