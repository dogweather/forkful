---
title:    "Python: Utvinna delsträngar"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar kan vara en användbar teknik för att manipulera textdata i Python. Det tillåter dig att enkelt hämta delar av en sträng som matchar ett visst mönster eller kriterium. Detta kan vara till nytta för många olika programmeringsuppgifter, som att formatera datum, filtrera data eller hantera användarinput.

## Hur man gör det

För att extrahera en substräng i Python, kan du använda metoden `.substring()` eller indexering på en sträng. Låt oss säga att vi har en sträng som representerar en personens namn och vi vill extrahera bara förnamnet. Vi kan göra det på följande sätt:

```python
namn = "Sofia Andersson"
förnamn = namn[0:5] # indexering från 0 till 5 ger oss "Sofia"
```

Om vi ville extrahera efternamnet, kan vi använda metoden `.substring()`:

```python
efternamn = namn.substring(6) # ger oss "Andersson"
```

För att extrahera en specifik del av en sträng, kan vi använda indexering med hjälp av konceptet "slicing". Till exempel, om vi bara vill ha det andra till det femte tecknet i en sträng, kan vi göra det på detta sätt:

```python
sträng = "Hej hej"
substring = sträng[1:5] # ger oss "ej h"
```

Det är också möjligt att använda indexering med negativa tal för att räkna bakifrån i en sträng. Till exempel, om vi ville extrahera de sista tre tecknen i en sträng kan vi göra det på följande sätt:

```python
sträng = "Hej hej"
substring = sträng[-3:] # ger oss "hej"
```

## Djupdykning

Det finns många olika sätt att använda substrängar i Python, men det är viktigt att notera att beroende på språket som används för strängmanipulering, så kan syntaxen och metoderna för att extrahera substrängar variera.

Förutom det som presenteras ovan, finns det mer avancerade sätt att extrahera substrängar i Python, som att använda reguljära uttryck och använda inbyggda funktioner som `.split()` eller `.partition()`.

Det är också viktigt att förstå att strängar i Python är oföränderliga, vilket innebär att de inte kan ändras direkt. Om du vill manipulera en sträng måste du skapa en kopia och sedan ändra den.

## Se även

- [Python String Methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [String slicing in Python](https://www.programiz.com/python-programming/methods/string/slice)
- [Regular expressions in Python](https://docs.python.org/3/library/re.html)