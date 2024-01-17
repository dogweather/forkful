---
title:                "Extrahering av delsträngar"
html_title:           "Python: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar innebär att man tar ut en del av en större sträng. Det är vanligt bland programmerare eftersom det är ett bra sätt att manipulera och hantera textdata på ett effektivt sätt.

## Hur man gör:
Det finns flera sätt att extrahera substrängar i Python, men det enklaste är att använda indexering och slices. Här är två exempel på hur man kan extrahera substrängar:

```Python
# Extrahera en del av strängen baserat på position
string = "Hello World"
substring = string[0:5] # resultat: "Hello"

# Extrahera en del av strängen baserat på värde
string = "This is a sentence"
substring = string[string.index("a"):string.index("sentence")] # resultat: "a sentence"
```

## Djupdykning:
Att extrahera substrängar har funnits i programmering sedan begynnelsen, och är ett vanligt verktyg för att manipulera och söka igenom textdata. Det finns också andra sätt att extrahera substrängar, som regelbundna uttryck och inbyggda funktioner. Det är viktigt att vara noga med indexering och slices för att få den exakta delen av strängen som man vill ha.

## Se även:
- https://python-docs-sv.readthedocs.io/dummy.html
- https://dummylink.com