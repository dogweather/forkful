---
title:                "Python: Att kapitalisera en sträng."
simple_title:         "Att kapitalisera en sträng."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att ha möjligheten att konvertera en sträng till stora bokstäver kan vara användbart när man vill göra en text mer lättläst och betona vissa ord eller fraser. Det är också en vanlig funktion inom datahantering, speciellt när man arbetar med strängar inom programmering.

## Hur man gör det

Det finns flera olika sätt att omvandla en sträng till stora bokstäver i Python, beroende på vilken version du använder. Här är två olika exempel på hur man kan göra det:

```Python
# Exempel 1: Med hjälp av inbyggda metoden upper()
text = "hej alla svenskar!"
print(text.upper()) # output: HEJ ALLA SVENSKAR!

# Exempel 2: Genom att loopa igenom varje tecken och omvandla dem
text = "välkommen till sverige!"
new_text = ""
for character in text:
    new_text += character.upper()
print(new_text) # output: VÄLKOMMEN TILL SVERIGE!
```

En annan viktig sak att komma ihåg är att när man arbetar med strängar i Python, är att de är immutable, vilket betyder att de inte kan ändras. Så när du omvandlar en sträng till stora bokstäver, kommer en ny sträng att skapas och den ursprungliga kommer att vara oförändrad.

## Djupdykning

Att kunna konvertera en sträng till stora bokstäver är en användbar funktion som kan användas i olika scenarier. Men det finns några saker att tänka på när man använder denna funktion:

- Omvandlingen kommer att påverka alla bokstäver i strängen, även siffror och specialtecken.
- Omvandlingen tar inte hänsyn till språk- och lätagiltiga regler. Till exempel kommer den tyska bokstaven "ß" att omvandlas till "SS".
- Omvandlingen kan påverka bokstäver som redan är i versaler, beroende på vilken metod som används.

Om du vill ha mer kontroll över konverteringen, finns det också möjlighet att använda sig av bibliotek som `unicodedata` för att hantera specialtecken och språkspecifika regler för versaler och gemener.

## Se även

- [Python 3 dokumentation: Strängmetoder](https://docs.python.org/sv/3/library/stdtypes.html#string-methods)
- [Real Python: How to Capitalize Strings in Python](https://realpython.com/python-string-formatting/)