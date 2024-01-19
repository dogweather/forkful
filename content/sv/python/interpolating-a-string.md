---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Stränginterpolation är tekniken att infoga variabler i en sträng. Det sparar tid för programmerare och gör koden tydligare.

## Hur man gör:

Python tillhandahåller flera sätt att interpolera strängar. Här kommer exempel på varje metod.

- Det äldre sättet att göra detta är att använda %-formatmetoden:

```Python
name = "Anna"
print("Hej %s!" % name)  # "Hej Anna!"
```

- Du kan också använda str.format-metod:

```Python
name = "Anna"
print("Hej {}!".format(name))  # "Hej Anna!"
```

- Eller det nya sättet med hjälp av s-strings (för Python3.6 och senare):

```Python
name = "Anna"
print(f"Hej {name}!")  # "Hej Anna!"
```

## Fördjupning 

Stränginterpolation har sin historia i språk som Perl och Ruby. Python införde det sedermera för att förenkla koden. Förutom de metoder som nämndes, det finns också en metod som kallas "Template Strings". Det är dock begränsat och används mindre frekvent.

S-strings (f-strings) är det modernaste sättet att interpolera strängar i Python. De är inte bara mer läsbara utan också snabbare eftersom de körs vid kompilering istället för körning.

## Se också:

För mer information, kolla följande länkar:

- Python Docs på strängformatering: https://docs.python.org/3/library/string.html
- PEP 3101, som introducerade str.format i Python 2.6: https://peps.python.org/pep-3101/
- PEP 498, som introducerade f-strings i Python 3.6: https://peps.python.org/pep-0498/