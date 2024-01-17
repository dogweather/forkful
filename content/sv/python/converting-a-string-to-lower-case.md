---
title:                "Omvandling av en sträng till gemener"
html_title:           "Python: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Konvertering av en sträng till små bokstäver är en vanlig uppgift inom programmering. Det innebär att alla bokstäver i en sträng görs om till små bokstäver istället för stora. Detta görs oftast för att underlätta jämförelse av strängar, då små och stora bokstäver annars behandlas som olika tecken i programmeringsspråk.

# Hur man:
```Python
my_string = "HALLÅ Det är en STRÄNG"
print(my_string.lower())
```
Output:
`hallå det är en sträng`

Genom att använda metoden `.lower()` på en sträng-variabel blir alla bokstäver i strängen omvandlade till små bokstäver. Detta gäller även för specialtecken och bokstäver med accent.

# Djupdykning:
Historiskt sett har omvandling av strängar till små bokstäver varit en komplicerad process. I äldre programmeringsspråk behövde man använda speciella funktioner eller loopar för att åstadkomma detta. Numera finns det inbyggda metoder i de flesta moderna språk som gör det betydligt enklare.

En annan metod för att skriva om strängar till små bokstäver är att använda modulen `string`. Där finns en funktion som heter `lower()` som också åstadkommer samma sak.

Tänk på att det kan finnas tillfällen då man vill behålla versaler i en sträng, till exempel för att betona ett ord eller en fras. Då är det viktigt att inte använda `.lower()` metoden och istället skriva strängen som den är.

# Se även:
- Python dokumentation för `string.lower()`: https://docs.python.org/3/library/string.html#string.lower
- En skriftlig guide på svenska över grundläggande koncept inom programmering: https://codingkungen.nu/