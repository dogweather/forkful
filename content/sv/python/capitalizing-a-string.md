---
title:                "Versaler för en sträng"
html_title:           "Python: Versaler för en sträng"
simple_title:         "Versaler för en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig operation inom programmering som kan användas för formatering av text eller för att skapa bättre läslighet i utdata.

## Hur man gör det

Det finns flera sätt att kapitalisera en sträng i Python, men det enklaste sättet är att använda inbyggda metoden "capitalize()". Här är ett exempel på hur man kan använda den:

```python
str = "hej världen"
str.capitalize()
print(str)

# Output: Hej världen
```

Man kan också använda metoderna "upper()" och "lower()" för att kapitalisera hela strängen eller göra den helt gemener, som i följande exempel:

```python
str = "exempel text"
str.upper()
print(str)

# Output: EXEMPEL TEXT

str.lower()
print(str)

# Output: exempel text
```

Om man vill kapitalisera en specifik del av strängen, som första bokstaven i varje ord, finns det en metod som heter "title()". Den här metoden kapitaliserar varje ord utifrån dess första bokstav, vilket kan vara användbart när man till exempel vill konvertera ett namn till titelcase:

```python
str = "john doe"
str.title()
print(str)

# Output: John Doe
```

## Djupdykning

När man kapitaliserar en sträng gör Python det genom att omvandla den första bokstaven till stor bokstav och resten av bokstäverna till små bokstäver. Detta kan dock skilja sig åt beroende på det språk som används. Till exempel, i det tyska språket finns det bokstaven "ß" som kan visas som "SS" i versaler. Om man använder "capitalize()" på en tysk sträng som innehåller denna bokstav, kommer den att kapitaliseras på det motsatta sättet.

Det är också viktigt att komma ihåg att string-metoderna "capitalize()", "upper()" och "lower()" returnerar en ny sträng istället för att ändra den ursprungliga strängen. Om man vill ändra den befintliga strängen måste man tilldela resultatet till en variabel eller använda metoden "str.replace()" istället.

## Se även

- [`String capitalize()` dokumentation](https://www.w3schools.com/python/ref_string_capitalize.asp)
- [`String upper()` dokumentation](https://www.w3schools.com/python/ref_string_upper.asp)
- [`String lower()` dokumentation](https://www.w3schools.com/python/ref_string_lower.asp)
- [`String title()` dokumentation](https://www.w3schools.com/python/ref_string_title.asp)