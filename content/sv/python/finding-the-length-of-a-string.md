---
title:                "Hitta längden av en sträng"
html_title:           "Python: Hitta längden av en sträng"
simple_title:         "Hitta längden av en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom programmering, och är användbar för att manipulera och hantera olika textsträngar. Genom att kunna hitta längden på en sträng kan man enkelt bestämma hur många tecken en sträng innehåller.

## Så här gör du

För att hitta längden på en sträng i Python, kan du använda funktionen `len()`. Den tar in en sträng som argument och returnerar längden på den strängen.

```python
sträng = "Hej alla! Jag heter Emma."
längd = len(sträng)
print(längd)

# Output: 24
```

I detta exempel har vi tilldelat en sträng till variabeln "sträng" och sedan använt funktionen `len()` för att räkna ut längden på denna sträng. Resultatet, 24, lagras i variabeln "längd" och sedan skrivs ut.

```python
längd = len("123456789")
print(längd)

# Output: 9
```

Du kan också använda `len()` för att räkna ut längden på ett helt nummer, förutsatt att det är ett godtyckligt objekt. I detta exempel har vi använt funktionen direkt på strängen "123456789" och resultatet blir då 9.

## Djupdykning

När vi använder funktionen `len()` på en sträng, räknar den antalet tecken i strängen, inklusive mellanslag, punktuering och specialtecken. Detta betyder att även tomma strängar har en längd på 0 karaktärer.

```python
tom_sträng = ""
print(len(tom_sträng))

# Output: 0
```

Det finns också andra metoder för att hitta längden på en sträng, såsom att använda en for-loop och iterera över varje tecken i strängen. Detta kan vara användbart om du vill utföra andra åtgärder på specifika tecken i strängen samtidigt som du räknar ut längden.

## Se även

- [Officiell dokumentation för Python - len()](https://docs.python.org/3/library/functions.html#len)
- [Enkel guide till Python](https://realpython.com/python-basics/) (på engelska)