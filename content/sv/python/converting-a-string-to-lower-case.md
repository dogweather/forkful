---
title:                "Omvandla en sträng till gemener"
html_title:           "Python: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift inom programmering, och det finns många praktiska användningsområden för detta. Det kan till exempel vara användbart när man ska söka eller filtrera igenom textdata, eller när man vill jämföra strängar oavsett deras skiftläge.

## Hur man gör det

För att konvertera en sträng till små bokstäver i Python, används den inbyggda funktionen `.lower()`. Det här exemplet visar hur man använder funktionen och skriver ut resultatet:
```Python
my_str = "Hej, Världen!"
print(my_str.lower())
```

Output:
```
hej, världen!
```

Denna funktion är också tillgänglig för andra datatyper som innehåller text, som t.ex. listor eller dictionaries. Det är viktigt att notera att funktionen inte påverkar originalsträngen, utan returnerar en ny sträng med små bokstäver.

## Djupdykning

Bakom kulisserna använder `.lower()` funktionen sig av Unicode-teckenkoder för att konvertera strängen till små bokstäver. Detta gör att den fungerar på alla språk, inklusive icke-latiniska alfabet. Om du vill lära dig mer om Unicode-tecken och deras användning inom programmering, kan du läsa mer här: https://docs.python.org/3/howto/unicode.html

## Se även

- Python 3 dokumentation: https://docs.python.org/3/library/stdtypes.html#str.lower
- "Capitalization in Different Languages" av Joel Lim: https://www.emreakkas.com/internationalization/capitalization-in-different-languages