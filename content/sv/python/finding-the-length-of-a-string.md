---
title:                "Att hitta längden på en sträng"
html_title:           "Python: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är en vanlig uppgift inom programmering. Det betyder helt enkelt att ta reda på hur många tecken en sträng består av. Detta är användbart när man behöver behandla text på olika sätt eller jämföra olika strängar med varandra.

## Så här gör du:
För att hitta längden på en sträng i Python, använder man funktionen "len()." Det räcker med att skriva detta korta kommando innanför parenteserna, precis som i exemplet nedan:

```Python
my_string = "Hej världen!"
print(len(my_string))
```

Detta kommer att returnera värdet 12 i terminalen, eftersom strängen "Hej världen!" består av 12 tecken.

För att hitta längden på en tom sträng, skriver man bara len() utan några argument innanför parenteserna:

```Python
empty_string = ""
print(len(empty_string))
```

Detta kommer att returnera värdet 0, eftersom en tom sträng inte innehåller några tecken alls.

## Djupdykning:
Att hitta längden på en sträng är en grundläggande och vanlig uppgift inom programmering. Det är också en effektiv metod för att jämföra strängar med varandra, eftersom man kan kontrollera om de har samma längd.

Innan len()-funktionen introducerades i Python, var det vanligt att använda en for-loop för att gå igenom varje tecken i en sträng och räkna dem manuellt. Men med len() blir detta enkelt och snabbt gjort med bara en rad kod.

Det finns också andra sätt att hitta längden på en sträng, till exempel genom att använda Indexing eller Slicing metoder. Men len() är den mest effektiva och enklaste metoden för att ta reda på längden på en sträng i Python.

## Se även:
Här är några länkar till andra resurser som kan vara användbara för att lära sig mer om att hitta längden på strängar i Python:

- [Python dokumentation om len()](https://docs.python.org/3/library/functions.html#len)
- [Tre olika sätt att räkna längden på en sträng i Python](https://www.geeksforgeeks.org/python-string-length-count/)
- [RealPython's guide om att räkna tecken i en sträng](https://realpython.com/python-string-length/)