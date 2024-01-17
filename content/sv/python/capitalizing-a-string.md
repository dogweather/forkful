---
title:                "Att omvandla en sträng till versaler"
html_title:           "Python: Att omvandla en sträng till versaler"
simple_title:         "Att omvandla en sträng till versaler"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man "capitalizar" en sträng i Python gör man den första bokstaven av varje ord större än de andra. Detta görs för att göra texten mer lättläslig och för att följa konventionen i de flesta språk där den första bokstaven i en mening är större.
 
## Hur gör man:
```python
# Exempel 1: Enkel
str = "hej världen!"
print(str.capitalize())
Output: "Hej världen!"

# Exempel 2: Med flera ord
str = "välkommen till python-världen"
print(str.capitalize())
Output: "Välkommen till python-världen"
```

## Djupdykning:
Historiskt sett har man använt metoden capitalize() för att göra den första bokstaven i en mening stor för att lägga till ett behagligt visuellt element. Alternativ till att använda metoden är att manuellt ändra bokstaven eller använda en regex regex för att ändra texten. Implementationen av capitalize() är enkel och kan lätt implementeras i egna funktioner eller program.

## Se även:
- [Python dokumentation för capitalize()](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Regex tutorial](https://www.regular-expressions.info/)