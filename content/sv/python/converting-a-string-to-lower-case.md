---
title:                "Python: Omvandling av en sträng till gemener"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en användbar funktion i Python för att enkelt hantera textdata. Det kan hjälpa till att skapa enhetliga och lättlästa strängar, samt underlätta för jämförelser och dataanalyser.

## Hur man gör det
I Python finns en inbyggd metod som heter `lower()` som utför omvandlingen av en sträng till små bokstäver. Den tar inga argument och returnerar en kopia av den ursprungliga strängen i lågskrift. Se nedan för ett exempel:

```Python
text = "HEJ DÄR!"
print(text.lower())
```

Output: `hej där!`

Det är viktigt att notera att denna metod inte ändrar den ursprungliga strängen, utan returnerar en ny kopia av den omvandlade strängen. Om man vill spara det nya värdet måste man alltså tilldela det till en variabel.

Man kan också använda `lower()` tillsammans med andra strängmetoder, som `split()` och `join()`, för att manipulera och bearbeta textdata på olika sätt. Det finns också möjlighet att använda sig av reguljära uttryck för mer avancerad bearbetning av textsträngar.

## Djupdykning
När man arbetar med stora mängder textdata kan det vara viktigt att hantera olika teckenkodningar och specialtecken. Vid konvertering av en sträng till små bokstäver kommer `lower()`-metoden endast att omvandla bokstäver som finns i det engelska alfabetet. Detta kan leda till problem om man har specialtecken eller tecken från andra språk i sin sträng.

Ett sätt att lösa detta är att använda sig av den inbyggda modulen `unicodedata` som erbjuder flera hjälpfunktioner för att hantera teckenkodningar. Genom att använda funktionen `normalize()`, tillsammans med argumentet `"NFKD"`, kan man omvandla strängen till en enhetlig Unicode-form innan man applicerar `lower()`. Därmed kommer även specialtecken att omvandlas korrekt till små bokstäver.

```Python
import unicodedata

text = "Är det här ett problem?"
print(unicodedata.normalize("NFKD", text).lower())
```

Output: `är det här ett problem?`

Det här är ett viktigt steg för att säkerställa att all text i en databas eller fil är enhetligt strukturerad för enklare hantering och analys.

## Se även
* [Python String Methods (engelska)](https://www.w3schools.com/python/python_ref_string.asp)
* [Unicode HOWTO (engelska)](https://docs.python.org/3/howto/unicode.html)
* [The Python Standard Library - unicodedata (engelska)](https://docs.python.org/3/library/unicodedata.html)