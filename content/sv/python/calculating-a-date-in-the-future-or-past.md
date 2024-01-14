---
title:                "Python: Beräkna ett datum i framtiden eller förflutna"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför 

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara användbart i många olika situationer, såsom att planera resor, födelsedagar eller viktiga händelser. Det kan också hjälpa till med bokföring eller projektplanering när man behöver veta när en viss tidsperiod börjar och slutar.

## Så här gör du 

För att kunna beräkna ett datum i framtiden eller det förflutna i Python behöver du först importera datetime-modulen. Sedan kan du använda funktionen `timedelta()` för att lägga till eller subtrahera en viss tidsperiod från ett datum. 

```Python 
import datetime 

today = datetime.date.today() 
print("Idag är det:", today) 

# Lägg till 30 dagar till dagens datum 
future_date = today + datetime.timedelta(days=30) 
print("Om 30 dagar är det:", future_date) 

# Subtrahera 2 veckor från dagens datum 
past_date = today - datetime.timedelta(weeks=2) 
print("För 2 veckor sedan var det:", past_date) 
```

Output:
```
Idag är det: 2021-10-10
Om 30 dagar är det: 2021-11-09
För 2 veckor sedan var det: 2021-09-26
```

## Djupdykning 

För att förstå hur datumen beräknas i koden ovan är det viktigt att förstå hur `timedelta()`-funktionen fungerar. Den tar emot parametrar för antal dagar, veckor, månader eller år som ska läggas till eller subtraheras från ett datum. Dessutom kan man även ange parameter för timmar, minuter, sekunder och mikrosekunder för mer exakta beräkningar. 

Det finns också andra användbara funktioner i datetime-modulen för att hantera datum och tid, såsom `date()` för att skapa ett datumobjekt, `time()` för att skapa ett tidsobjekt, och `datetime()` för att skapa ett datum och tidsobjekt. 

Det finns många olika sätt att använda datetime-modulen och dess funktioner för att beräkna datum i framtiden eller det förflutna. Genom att öva och utforska ytterligare kan du lära dig fler avancerade tekniker för hantering av datum och tid i Python. 

## Se också 

- [Python's Official Documentation on datetime](https://docs.python.org/3/library/datetime.html) (engelska)
- [Python SE:s tutorial om datetime](https://python.se/tutorials/verktyg/tidsritten-views.py) (svenska)
- [W3Schools' tutorial om datetime](https://www.w3schools.com/python/python_datetime.asp) (engelska)