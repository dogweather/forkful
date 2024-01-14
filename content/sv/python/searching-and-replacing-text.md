---
title:                "Python: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför?

Att söka och ersätta text är en vanlig uppgift inom programmering. Det kan vara användbart för att korrigera stavfel, ändra variabler eller uppdatera innehållet i en fil. Genom att använda Python kan vi enkelt automatisera denna uppgift och spara tid och ansträngning.

## Hur man gör det

I Python finns det flera sätt att söka och ersätta text. Den enklaste metoden är att använda replace() funktionen, vilket ersätter en viss sträng med en annan.

```Python
text = "Hej! Jag heter Emma."
ny_text = text.replace("Emma", "Lisa")
print(ny_text)
# Output: Hej! Jag heter Lisa.
```

För att byta ut flera förekomster av en sträng, kan vi använda replace() funktionen i en loop.

```Python
text = "Det finns en stor hund, en liten hund och en medelstor hund."
hundar = ["stor", "liten", "medelstor"]
for hund in hundar:
  ny_text = text.replace(hund, "gammal")
  print(ny_text)

# Output: Det finns en gammal hund, en gammal hund och en gammal hund.
```

En annan användbar metod är att använda regular expressions (regex) för att söka efter ett mönster och ersätta det med en annan sträng.

```Python
import re
text = "Min favoritfärg är blå, men jag gillar också grönt."
ny_text = re.sub("blå|gillar", "röd", text)
print(ny_text)

# Output: Min favoritfärg är röd, men jag tycker också om grönt.
```

## Djupdykning

När vi söker och ersätter text med hjälp av regex, kan vi använda specifika metakaraktärer för att uttrycka ett mer komplicerat mönster. Till exempel:

- \w: matchar alla bokstäver och siffror.
- \d: matchar alla siffror.
- \s: matchar alla mellanslag och tabbar.
- .: matchar alla tecken (inklusive specialtecken).

Vi kan också använda dessa metakaraktärer tillsammans med regex kvantitetsuttryck för att göra vårt mönster ännu mer specifikt. Till exempel:

- ?: matchar 0 eller 1 gånger.
- *: matchar 0 eller fler gånger.
- +: matchar 1 eller fler gånger.
- {n}: matchar exakt n antal gånger.
- {n,m}: matchar minst n och högst m antal gånger.

Låt oss använda dessa kunskaper för att ersätta all text inom parenteser med en annan sträng.

```Python
import re
text = "Namn: Emma (19)"
ny_text = re.sub("\(.+?\)", "(25)", text)
print(ny_text)

# Output: Namn: Emma (25)
```

Som vi kan se i exemplet ovan, använde vi metakaraktären ? för att matcha "19" endast en gång, och ersatte det sedan med "25". Genom att lära oss mer om regex och dess metakaraktärer kan vi göra ännu mer avancerade sökningar och ersättningar i text.

## Se även

- [Python regex dokumentation](https://docs.python.org/3/library/re.html)
- [Tutorial för regex](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)