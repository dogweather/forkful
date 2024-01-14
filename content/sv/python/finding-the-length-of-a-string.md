---
title:    "Python: Att hitta längden på en sträng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en viktig del av programmering eftersom det ofta behövs när man arbetar med textdata. Det är också användbart för att kunna utföra olika manipulationer på strängen, som att till exempel hitta specifika tecken eller ta ut delar av strängen.

## Hur man gör

Det finns flera sätt att hitta längden på en sträng i Python, men den enklaste metoden är att använda den inbyggda funktionen `len()`. Denna funktion tar en sträng som argument och returnerar längden på strängen som ett heltal. Låt oss titta på ett exempel:

```Python
sträng = "Hej, världen!"
print(len(sträng))
```

Output:

```
13
```

Som vi kan se så returnerar `len()` funktionen längden på strängen, inklusive mellanslag och skiljetecken.

En annan metod för att hitta längden på en sträng är att använda en loop för att räkna antalet tecken. Till exempel:

```Python
sträng = "Hej, världen!"
count = 0

for char in sträng:
    count += 1

print(count)
```

Output:

```
13
```

Detta fungerar genom att loopa genom varje tecken i strängen och öka räknaren med 1 för varje tecken. När loopen är klar så kommer räknaren ha räknat antalet tecken, vilket i detta fall är längden på strängen.

## Djupdykning

Att hitta längden på en sträng kan verka enkelt, men det finns faktiskt mer komplexitet bakom det. I Python så är en sträng egentligen bara en sekvens av tecken, vilket betyder att det också är möjligt att använda list-indexering för att komma åt enskilda tecken eller delar av strängen. Detta innebär att längden på en sträng också kan användas för att hitta den sista karaktären i strängen genom att använda en negativ index. Till exempel:

```Python
sträng = "Hej, världen!"
sista_karaktären = sträng[-1]

print(sista_karaktären)
```

Output:

```
!
```

Man kan också använda `len()` funktionen för att kontrollera att en sträng innehåller ett visst antal tecken innan man försöker komma åt dem med list-indexering.

## Se även

- [Python Dokumentation - String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [W3Schools - Python String Methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [GeeksforGeeks - How to get string length in Python](https://www.geeksforgeeks.org/get-string-length-in-python/)