---
title:                "Konvertera ett datum till en sträng"
html_title:           "Python: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng innebär att omvandla ett datumvärde till en textsträng. Detta är ett vanligt behov för programmerare eftersom det gör datumet mer läsbart och användbart i vissa situationer, som att spara datumet i en fil eller skriva det ut på skärmen.

## Så här gör du:
```Python
# Importera datetime-modulen
import datetime

# Definiera ett datumvärde
datum = datetime.date(2020, 6, 15)

# Konvertera datumet till en sträng med formatet ÅÅÅÅ-MM-DD
sträng = datum.strftime("%Y-%m-%d")

# Skriv ut den konverterade strängen
print(sträng)

# Resultat: 2020-06-15
```

## Djupdykning:
Historiskt sett hade programmerare olika metoder för att hantera datum, såsom att använda heltal eller specifika tecken för varje del av ett datum. Detta gjorde det svårt att hantera och jämföra datum. Sedan introduktionen av datetime-modulen i Python, har det blivit enklare och mer flexibelt att konvertera datum till strängar och utföra olika operationer på dem.

Det finns också alternativ till datetime-modulen, såsom dateutil och arrow, som erbjuder mer avancerade funktioner för att hantera datum och tider. Det är viktigt att välja det rätta verktyget beroende på vad du behöver göra med dina datum.

Vid konvertering av datum till sträng är det viktigt att förstå formateringssträngen som används för att definiera hur datumet ska visas. Till exempel ger "%Y-%m-%d" formatet för år-månad-dag, medan "%d/%m/%Y" ger formatet för dag/månad/år. Det finns många olika format att välja mellan och det är viktigt att använda rätt format för ditt specifika syfte.

## Se även:
- [Python datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [dateutil](https://dateutil.readthedocs.io/en/stable/)
- [arrow](https://arrow.readthedocs.io/en/latest/)