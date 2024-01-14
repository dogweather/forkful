---
title:                "Python: Kapitalisera en sträng"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att prata om hur man kapitaliserar en sträng i Python. Det kan låta som en enkel uppgift, men det finns faktiskt flera olika sätt att göra det på och det kan vara användbart i olika programmeringsscenarier.

## Så här gör du

För att kapitalisera en sträng i Python kan du använda antingen inbyggda metoder eller skriva din egen funktion. Här är ett exempel på hur du kan göra det med inbyggda metoderna `upper()` och `capitalize()`:

```Python
# Skapa en sträng:
string = "hej, jag är en sträng"

# Använda metoden upper() för att kapitalisera hela strängen:
print(string.upper()) # HEJ, JAG ÄR EN STRÄNG

# Använda metoden capitalize() för att kapitalisera första bokstaven:
print(string.capitalize()) # Hej, jag är en sträng
```

Om du vill skriva din egen funktion kan du använda dig av `split()` för att dela upp strängen i ord och sedan använda `capitalize()` på varje ord. Här är ett exempel på en sådan funktion:

```Python
def capitalize_string(string):
    # Dela upp strängen i ord:
    words = string.split()

    # Skapa en tom lista för de kapitaliserade orden:
    capitalized_words = []

    # Loopa igenom orden och använda capitalize() på varje ord:
    for word in words:
        capitalized_words.append(word.capitalize())

    # Returnera de kapitaliserade orden som en sträng:
    return " ".join(capitalized_words)

# Använda den egna funktionen på vår sträng:
print(capitalize_string(string)) # Hej, Jag Är En Sträng
```

Som du kan se i koden ovan finns det olika sätt att kapitalisera en sträng i Python, så det är viktigt att hitta det sätt som passar bäst för ditt specifika projekt och behov.

## Gå djupare

För att förstå hur `upper()` och `capitalize()` fungerar i Python är det bra att ha en grundläggande förståelse för strängar och metoder. En sträng är i grunden en sekvens av tecken, och genom att använda inbyggda metoder kan vi ändra och manipulera dessa tecken på olika sätt. `upper()` kommer, som namnet antyder, kapitalisera hela strängen och `capitalize()` kommer att endast kapitalisera första bokstaven. Men det finns många andra metoder som kan vara användbara vid arbetet med strängar, som till exempel `lower()`, `replace()` och `strip()`.

## Se även

- [Python.org - String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Tutorialspoint - String Methods in Python](https://www.tutorialspoint.com/python/python_strings.htm)
- [Real Python - Working with Strings in Python 3](https://realpython.com/python-strings/)