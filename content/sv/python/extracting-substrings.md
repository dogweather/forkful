---
title:                "Python: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Är du nyfiken på hur man kan hämta ut delsträngar från en sträng i Python? Då har du kommit rätt! Läs vidare för att lära dig mer om denna användbara kodteknik.

## Hur man gör det

För att hämta ut delsträngar från en sträng i Python kan vi använda funktionen `substring()`. Se exemplet nedan för att se hur den kan användas i praktiken:

```Python
# Skapa en sträng
str = "Detta är en sträng"

# Hämta en del av strängen från index 8 till slutet av strängen
delstr = str[8:]

# Skriv ut delsträngen
print(delstr)

# Output: en sträng
```

Som du kan se är det enkelt och smidigt att hämta delsträngar från en sträng i Python. Genom att använda index kan du välja vilka delar av strängen du vill hämta ut.

## Djupdykning

För att förstå hur `substring()`-funktionen fungerar i Python, behöver vi först förstå konceptet med index. I Python börjar index alltid på 0, vilket betyder att den första karaktären i en sträng har index 0, den andra har index 1 och så vidare.

När vi använder `str[start:end]` i en sträng, hämtar vi en delsträng från den givna startpositionen till slutet av strängen eller till den givna slutpositionen. Om end inte är specificerat kommer delsträngen att inkludera alla karaktärer från och med startpositionen till slutet av strängen.

Vi kan också använda negativa index för att hämta delsträngar. Om vi använder `str[start:-end]` kommer delsträngen att inkludera alla karaktärer från startpositionen till slutet av strängen minus de sista n karaktärerna, där n är värdet av end.

## Se även

- [String Methods in Python](https://www.programiz.com/python-programming/methods/string)
- [How to Extract Substrings in Python](https://www.datacamp.com/community/tutorials/python-string-methods)
- [Python String Methods Cheat Sheet](https://www.pythonforbeginners.com/cheatsheet/python-string-methods-cheat-sheet)