---
title:                "Python: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en viktig färdighet inom Python-programmering. Genom att kunna göra det kan du manipulera och bearbeta data på ett effektivt sätt.

## Hur man gör

Att hitta längden på en sträng i Python är enkelt och kan göras på flera olika sätt.

### Metod 1: Inbyggd funktion `len()`

Det enklaste sättet att hitta längden på en sträng är genom att använda den inbyggda `len()` funktionen. Denna funktion tar in en sträng som argument och returnerar dess längd som ett heltal.

```Python
str = "Hej! Jag heter Python"

print(len(str))
```

Kodblocket ovan kommer att skriva ut längden på strängen till konsolen, vilket i detta fall är 21.

### Metod 2: Loop

En annan metod för att hitta längden på en sträng är genom att använda en loop. Vi kan loopa igenom varje tecken i strängen och hålla räkningen med en variabel.

```Python
str = "Hej! Jag heter Python"
length = 0

for char in str:
    length += 1
    
print(length)
```

Här ökar vi värdet på vår variabel `length` med 1 för varje tecken vi loopar igenom, vilket ger oss längden på strängen.

### Metod 3: Använda index

Vi kan också använda index för att hitta längden på en sträng. Vi kan enkelt få det sista tecknet i strängen genom att använda `str[-1]`. Genom att sedan loopa genom indexen och öka värdet på en variabel får vi längden på strängen.

```Python
str = "Hej! Jag heter Python"
length = 0

while str[length:]:
    length += 1
    
print(length)
```

## Djupdykning

I Python är en sträng en sekvens av tecken som är indexerade från 0 till längden på strängen minus 1. Detta gör det möjligt för oss att använda index för att hitta längden på en sträng.

Det finns också andra inbyggda funktioner som kan göra mer komplicerade operationer relaterade till stränglängder, såsom `count()`, `find()` och `split()`. Genom att förstå hur man hittar längden på en sträng kan du enkelt använda dessa funktioner för att manipulera och analysera data.

Du kan också använda `len()` funktionen tillsammans med andra datatyper som listor och tupler för att hitta deras längder.

## Se även

* [Python's oficialida dokumentation om strängar](https://docs.python.org/sv/latest/tutorial/introduction.html#strings)
* [Python grundläggande strängoperationer](https://www.programiz.com/python-programming/string)
* [Inbyggda funktioner i Python](https://www.w3schools.com/python/python_ref_functions.asp)