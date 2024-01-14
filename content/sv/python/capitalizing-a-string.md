---
title:    "Python: Att stor bokstavera en sträng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Funderar du på att lära dig hur du kan stora bokstäver i en sträng i Python? Det finns många olika situationer där detta kan vara användbart, till exempel när du vill ändra en titel till att börja med stora bokstäver eller när du vill formatera användarnas inmatning på ett enhetligt sätt.

## Hur man gör det

För att stora bokstäver i en sträng i Python behöver du använda en inbyggd funktion som heter "capitalize". Det här är en enkel funktion som du enkelt kan lära dig att använda.

```python
text = "hej där!"
print(text.capitalize())
```

Output: "Hej där!"

Som du kan se, kapitaliserar funktionen det första tecknet i strängen, medan resten av tecknen förblir oförändrade.

För att stora bokstäver på alla ord i en sträng, kan du istället använda funktionen "title". Detta är användbart om du vill format ett namn eller en titel på ett korrekt sätt.

```python
text = "det här är en rubrik"
print(text.title())
```

Output: "Det Här Är En Rubrik"

## Djupdykning

Förutom "capitalize" och "title" har Python också funktioner som "upper" och "lower" för att omvandla en sträng till bara stora eller små bokstäver. Du kan också använda slicing-tekniker för att stora bokstäver på specifika delar av en sträng.

Det finns också möjligheter att formatera en sträng med hjälp av "format" funktionen. Detta är särskilt användbart om du vill skapa komplexa eller dynamiska strängar med variabler.

## Se också

- [Python's capitalize funktion](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [En jämförelse av funktionerna capitalize och title](https://www.geeksforgeeks.org/python-string-capitalize-vs-title/)
- [Mer om sträng formatering i Python](https://realpython.com/python-f-strings/)