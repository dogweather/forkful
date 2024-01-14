---
title:                "Python: Extrahera delsträngar"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Substringutvinning, eller att extrahera en del av en sträng, är en mycket användbar funktion i Python för att manipulera och behandla textdata. Det gör det möjligt för användare att enkelt hitta och manipulera specifika delar av en textsträng, vilket kan spara tid och göra koden mer effektiv.

## Hur man gör

För att extrahera substrings i Python, använda sig av slice-funktionen med följande syntax: `[start:stop:step]`. Startargumentet specificerar var substrängen ska börja och stopargumentet specificerar var den ska sluta. Stegargumentet används för att välja hur många tecken som ska hoppas över i varje steg.

```python
# Exempel på substrängutvinning från en variabel:
text = "Det här är en textsträng"
print(text[4:10])              # Output: är en
print(text[:7])                 # Output: Det här
print(text[11:])               # Output: textsträng
```

```python
# Substrängutvinning från en given text:
print("Python är ett populärt programmeringsspråk"[7:13])       # Output: populärt
```

## Djupdykning

Förutom slicefunktionen kan substrängutvinning också göras med hjälp av de inbyggda metoderna `find()` och `index()`. Dessa metoder söker igenom en sträng för en given delsträng och returnerar indexet för den första förekomsten. Om delsträngen inte hittas, returneras -1.

```python
# Exempel på substrängutvinning med `find()` och `index()`:
text = "Denna textsträng innehåller igen"
print(text.find("inne"))    # Output: 20
print(text.index("innehåller"))  # Output: 18
print(text.find("finns inte"))   # Output: -1
print(text.index("finns inte"))  # Raises error
```

Substrängutvinning är också användbar för att manipulera och omvandla textsträngar. Till exempel, om man vill byta ut ett visst ord eller tecken med ett annat, kan man använda sig av slicefunktionen och konkatenera strängarna.

```python
# Exempel på manipulering av text med substrängutvinning:
text = "Jag gillar bananer"
ny_text = text[:5] + "älskar" + text[5:12]
print(ny_text)      # Output: Jag älskar bananer
```

## Se även

- [Python Slice Notation](https://docs.python.org/3/tutorial/introduction.html#strings)
- [String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [The Power of Slicing in Python](https://www.geeksforgeeks.org/python-strings/)