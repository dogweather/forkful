---
title:                "Få den aktuella datumet."
html_title:           "Python: Få den aktuella datumet."
simple_title:         "Få den aktuella datumet."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den nuvarande datumet är en viktig del av programmering eftersom det möjliggör att spåra när en viss kod utfördes eller när data genererades. Det är också användbart för att hantera tidsrelaterade funktioner och funktioner för planerade uppgifter.

## Så här gör du:
Det finns flera olika sätt att få den nuvarande datumet i Python. Ett sätt är att använda modulen "datetime" och dess funktion "now ()" för att returnera ett datumobjekt som representerar den nuvarande datum och tid. Detta kan göras enligt följande kod:

```Python
import datetime

current_date = datetime.datetime.now()
print(current_date)
```

Output:
`2021-05-10 10:17:09.158581`

Ett annat sätt är att använda modulen "date" och funktionen "today ()" för att bara returnera datumet utan tid. Det kan åstadkommas så här:

```Python
import datetime

current_date = datetime.date.today()
print(current_date)
```

Output:
`2021-05-10`

## Djupdykning:
Datum och tider är viktiga delar av programmering och har en lång historia av utveckling och standardisering. I Python används standardbiblioteket "datetime" för att hantera datum och tider. Alternativ till detta är externa bibliotek som "arrow" och "pendulum" som erbjuder mer avancerade funktioner för datumhantering.

Det är också viktigt att förstå att den nuvarande datumet kan påverkas av olika faktorer som tidszoner och systemets inställningar. Därför är det bäst att använda en pålitlig tidsmodul som "datetime" för att få den korrekta nuvarande datumet.

## Se även:
- [Datetime-modulen i Pythons officiella dokumentation](https://docs.python.org/3/library/datetime.html)
- [Arrow-biblioteket för avancerad datumhantering i Python](https://arrow.readthedocs.io/en/latest/)
- [Pendulum-biblioteket för mer intuitivt datum- och tidshantering](https://pendulum.eustace.io/)