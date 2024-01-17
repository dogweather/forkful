---
title:                "Stor bokstavering av en streng"
html_title:           "Python: Stor bokstavering av en streng"
simple_title:         "Stor bokstavering av en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Hva & Hvorfor? 
Når vi snakker om å kapitalisere en streng, mener vi å konvertere den første bokstaven i hvert ord til en stor bokstav. Dette gjøres vanligvis for å gjøre teksten mer leselig og estetisk tiltalende.

Programmerere bruker ofte denne funksjonen for å formatere tekst som skal vises til brukeren, for eksempel navn og titler.

## Slik gjør du det:
```Python
navn = 'andrea johansen'
print(navn.capitalize())
```
Output: Andrea Johansen

```Python
navn = 'john smith'
print(navn.title())
```
Output: John Smith

## Dypdykk:
Å kapitalisere en streng er ikke en ny praksis - det har vært brukt gjennom historien for å fremheve viktige navn og titler. I moderne programmering finnes det alternativer som ```str.upper()``` som konverterer hele strengen til store bokstaver og ```str.lower()``` som gjør det motsatte. 

Implementeringen av ```str.capitalize()``` i Python følger Unicode-spesifikasjonen og vil kun endre den første bokstaven i hvert ord, uavhengig av språket som brukes. Det er også mulig å kapitalisere en tekststreng i HTML ved hjelp av CSS.

## Se også:
- [Unicode karaktertabell](https://unicode-table.com/no/)
- [Python string methods dokumentasjon](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [CSS text-transform](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform)