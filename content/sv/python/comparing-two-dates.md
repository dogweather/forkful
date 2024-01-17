---
title:                "Jämföra två datum"
html_title:           "Python: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Jämförelse av två datum är en vanlig programmeringsuppgift där två datum jämförs för att se om de är lika eller om ett datum är tidigare eller senare än det andra. Detta är användbart för att sortera datum i en viss ordning eller för att kontrollera om ett datum har passerat.

## Hur man gör:

```Python
# Skapar två olika datum objekt
datum_1 = datetime.datetime(2021, 8, 23)
datum_2 = datetime.datetime(2021, 8, 25)

# Jämför om det första datumet är före det andra datumet
print(datum_1 < datum_2) # Output: True

# Jämför om de båda datumen är lika
print(datum_1 == datum_2) # Output: False
```

## Djupdykning:

Jämförelse av datum är en viktig del av datalogi och används ofta för att hantera tidsstämplar i program. Innan Python 2.4 fanns det inte något inbyggt sätt att jämföra datum, men sedan dess har PythonDateTime-modulen introducerat funktioner för detta ändamål. Det finns också andra alternativ som att använda ögonblick eller Unix-tid för att jämföra datum. I Python används vanligtvis ekvivalensen och ojämförlighetstecken (<,>, ==) för jämförelser.

## Se även:

- [Python Jämförelse av olika datum](https://docs.python.org/3/library/datetime.html#datetime.datetime)
- [Jämförelse av datum i andra programmeringsspråk](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(date_and_time))