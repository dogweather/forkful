---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:51:36.265527-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stränginterpolering är sättet att infoga värden i en sträng. Det används för att skapa dynamiska texter baserade på variabler, vilket är effektivt i både kodtydlighet och underhåll.

## Hur man gör:
```python
# Använd f-strängar för Python 3.6+
namn = "Erik"
yrke = "utvecklare"
meddelande = f"Hej, jag heter {namn} och jag är en {yrke}."
print(meddelande)

# Output: Hej, jag heter Erik och jag är en utvecklare.

# Äldre metod med str.format()
meddelande2 = "Hej, jag heter {} och jag är en {}.".format(namn, yrke)
print(meddelande2)

# Output: Hej, jag heter Erik och jag är en utvecklare.
```

## Djupdykning
Innan f-strängar introducerades i Python 3.6, användes `str.format()` och % operatorn (`"Hej, jag heter %s" % namn`) för stränginterpolering. Dessa metoder fungerar fortfarande, men f-strängar är mer lättlästa och presterar bättre. Interpoleringen sker i runtime, och f-strängar tillåter till och med uttryck inne i klammerparenteserna, vilket kan vara användbart för att direkt formatera data.

## Se även
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- [Python 3.9.1 documentation – Formatted string literals](https://docs.python.org/3/tutorial/inputoutput.html#formatted-string-literals)
- [Python 3.9.1 documentation – str.format()](https://docs.python.org/3/library/stdtypes.html#str.format)
