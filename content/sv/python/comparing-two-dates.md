---
title:    "Python: Jämföring av två datum"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av många programmeringsprojekt. Det kan hjälpa dig att sortera och filtrera data, beräkna tidsintervall och upptäcka mönster över tid.

## Hur man gör

För att jämföra två datum i Python kan du använda modulen `datetime`. Här är ett exempel på hur du kan jämföra två datum och få ut skillnaden mellan dem:

```Python
from datetime import date

datum1 = date(2020, 1, 1)
datum2 = date(2020, 1, 15)

skillnad = datum2 - datum1

print(f"Skillnaden mellan datum1 och datum2 är {skillnad.days} dagar.")
```

Detta kommer att producera följande utdata:

```
Skillnaden mellan datum1 och datum2 är 14 dagar.
```

## Djupdykning

När du jämför två datum måste du vara medveten om att datumen måste vara av samma typ. Det betyder att båda måste vara antingen `date`-objekt eller `datetime`-objekt. Om du behöver jämföra ett `date`-objekt med ett `datetime`-objekt, kommer du att få ett felmeddelande.

En annan viktig sak att tänka på är att datum inte kan jämföras med strängar. Om du har data i strängformat måste du konvertera den till ett `date`-objekt med hjälp av `strptime`-funktionen innan du kan jämföra det med ett annat datum.

En vanlig användning av jämförelse av datum är att kontrollera om ett datum är före eller efter ett annat datum. För detta kan du använda operatorerna `<` och `>` och få ett booleskt värde `True` eller `False`.

## Se också

- [Python datetime-dokumentation](https://docs.python.org/3/library/datetime.html)
- [Guide till att arbeta med datum och tider i Python](https://realpython.com/python-datetime/)