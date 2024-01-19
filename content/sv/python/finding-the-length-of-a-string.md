---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Söka längden på en sträng betyder att få reda på antalet tecken den innehåller. Programmerare behöver denna information när de behandlar textinmatning, utskriftsformat och dataanalys.

## Så här gör man:
Med Python kan du enkelt hitta strängens längd med `len()` funktionen. 

```Python 
sträng = 'Hej, Sverige!'
print(len(sträng))
```

Resultatet blir:

```
14
```

Detta visas eftersom vår sträng innehåller totalt 14 tecken.

## Djupdykning
Python's `len()` funktion introducerades redan i Python 1.0. Det är tillgängligt i samtliga versioner och är kärnfunktion i språket, vilket gör det extremt snabbt och effektivt. 

Alternativt kan du använda en for-loop för att räkna tecknen, men det kommer att vara långsammare än `len()`. 

```Python 
sträng = 'Hej, Sverige!'
längd = 0
for tecken in sträng:
  längd += 1
print(längd)
```

Det betyder inte att `len()` är alltid det bästa valet. Om du bara vill veta om strängen är tom eller inte, kanske `sträng == ''` är mer lämpligt och snabbare.

Implementationen av `len()` i Python använder C-stilen att lagra strängar: en null-tecken avslutar strängen, och `len()` fungerar genom att hoppa fram till nollstället och räkna hur många hopp den behövde.

## Se även:
- Python officiella dokumentation om `len()` funktionen: https://docs.python.org/3/library/functions.html#len
- En omfattande guide om hur man arbetar med strängar i Python: https://realpython.com/python-strings/
- StackOverflow fråga om olika sätt att mäta längden på en sträng: https://stackoverflow.com/questions/1735952/whats-the-fastest-way-in-python-to-measure-the-length-of-a-string