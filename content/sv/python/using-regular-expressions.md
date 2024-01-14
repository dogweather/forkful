---
title:                "Python: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions, eller reguljära uttryck, är ett användbart verktyg inom Python programmering som hjälper till att söka, identifiera och manipulera textsträngar. Det är ett kraftfullt verktyg som sparar tid och gör det möjligt att bearbeta stora mängder data snabbt och effektivt.

## Så här gör du
Det finns olika moduler inom Python som stöder regular expressions, bland annat "re" och "regex". För att använda dessa moduler måste de importeras i koden med hjälp av "import" kommandot. Här är ett exempel på hur man kan söka efter en viss textsträng i en variabel med hjälp av reguljära uttryck:

```Python
import re # importera modulen

text = "Detta är en textsträng som innehåller ordet Python"

match = re.search(r"Python", text) # sök efter ordet "Python" i textsträngen

if match:
    print("Ordet hittades.")
else:
    print("Ordet hittades inte.")
```

Output:

```
Ordet hittades.
```

I detta exempel används "re.search()" funktionen för att söka efter ett specifikt mönster, i detta fall ordet "Python". Om mönstret hittas, returneras en matchning och annars returneras "None". Notera att "r" prefixet före strängen används för att behandla strängen som ett reguljärt uttryck.

## Djupgående
Regular expressions erbjuder många olika mönster för sökning, så som sökning efter flera mönster, avläsning av specifika tecken och ersättning av textsträngar. Det finns också en mängd specialtecken som kan användas för att göra mer avancerade sökningar.

En av de viktigaste delarna av att använda regular expressions är att förstå hur dessa specialtecken fungerar. "```\d```" står till exempel för att söka efter en siffra, "```\w```" står för ett alfanumeriskt tecken och "```\s```" står för ett mellanslagstecken.

Det finns också olika funktioner som kan användas för att manipulera strängar med hjälp av regular expressions, så som "sub()", som ersätter matchade mönster med annan text.

För djupgående information och exempel på andra mönster och funktioner, se dokumentationen för "re" eller "regex" modulen.

## Se även
- [Re-modulen](https://docs.python.org/3/library/re.html)
- [Regex-modulen](https://docs.python.org/3/library/re.html)
- [Reguljära uttryck i Python](https://realpython.com/regex-python/)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)