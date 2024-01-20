---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver innebär att vi ändrar alla versala bokstäver i en sträng till gemena. Programmerare gör det när de vill standardisera, sortera eller jämföra strängar, oavsett användarens inmatning.

## Hur gör man:

Det är så enkelt att konvertera en sträng till gemena i Python. Du använder `lower()` metoden. Kolla på denna illustration:

```python
text = "Hej Världen!"
lower_text = text.lower()
print(lower_text)
```

Och det kommer att skriva ut:

```
hej världen!
```

I detta exempel, `lower()` metoden tar strängen `"Hej Världen!"` och konverterar den till `"hej världen!"`. 

## Djupdykning:

Att konvertera strängar till gemena är ingen ny idé, och det är standard i många programmeringsspråk för att hantera stränginformation. 

Det finns också alternativ till att använda `lower()` i Python. Du kan, till exempel, använda en `for`-loop och `ord()`-funktionen. Men `lower()` är mest effektiv och lätt att använda.

Slutligen, vad gäller implementation, Python använder ASCII eller Unicode-tabeller (beroende på strängtyp) för att konvertera versala till gemena.

## Se också:

Du kan också vara intresserad av att lära dig att konvertera strängar till versaler i Python. Kolla in [denna guide](https://www.w3schools.com/python/ref_string_upper.asp). Dessutom, om du vill utforska mer om Python's `lower()` metod och andra strängmetoder, har Python's officiella dokumentation massor av information [här](https://docs.python.org/3/library/stdtypes.html#str.lower).