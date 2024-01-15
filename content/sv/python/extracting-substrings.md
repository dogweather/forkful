---
title:                "Extrahera substrängar"
html_title:           "Python: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar (delsträngar) är en användbar teknik inom programmering för att få ut specifika delar av en längre sträng eller text. Det gör det möjligt att bearbeta och hantera data på ett mer effektivt sätt.

## Så här

För att extrahera en substräng i Python, används en kombination av inbyggda funktioner och indexering. Här är ett exempel på hur man kan extrahera den tredje och fjärde bokstaven i en sträng:

```Python
my_string = "Hej, jag heter Python"
substr = my_string[2:4]
print(substr)
```
Output: "j, "

Notera att indexering i Python startar på noll, så det andra argumentet i `my_string[2:4]` är egentligen den fjärde bokstaven i strängen.

Det är också möjligt att extrahera en substräng med hjälp av negativ indexering. Till exempel, om vi bara vill ha de två sista bokstäverna i `my_string`:

```Python
substr = my_string[-2:]
print(substr)
```

Output: "on"

För att extrahera en del av en sträng baserat på ett visst villkor, som till exempel att extrahera alla siffror från en sträng, kan man använda sig av en loop tillsammans med inbyggda metoden `isdigit()`, som returnerar True om tecknet är en siffra. Här är ett exempel på hur man kan genomföra detta:

```Python
my_string = "1 hem, 2 barn, 3 äpplen, 4 bananer"
numbers = ""
for char in my_string:
    if char.isdigit():
        numbers += char
print(numbers)
```

Output: "1234"

## Djupdykning

En viktig sak att komma ihåg när man extraherar substrängar är att de returneras som nya strängar och originalsträngen förblir oförändrad. Om man vill ändra på en sträng, måste man använda metoden `replace()`.

En annan användbar funktion för att extrahera substrängar är `find()`, som hittar positionen för en viss delsträng i en given sträng. Om substrängen inte finns, returnerar funktionen -1. Här är ett exempel:

```Python
my_string = "Hej, jag heter Python"
index = my_string.find("heter")
print(index)
```

Output: 12

Man kan också använda `find()` för att hitta alla förekomster av en delsträng och returnera en lista över deras indexpositioner:

```Python
my_string = "1 hem, 2 barn, 3 äpplen, 4 bananer"
positions = []
index = 0
while index < len(my_string):
    index = my_string.find("en", index)
    if index == -1:
        break
    positions.append(index)
    index += 1

print(positions)
```

Output: [2, 11, 16, 31]

## Se även

- [String Methods in Python (official documentation)](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python String Slicing (GeeksforGeeks article)](https://www.geeksforgeeks.org/python-string-slicing/)
- [Python String manipulation – replace, join, reverse, find, lower, upper (RealPython article)](https://realpython.com/python-string-manipulation/)