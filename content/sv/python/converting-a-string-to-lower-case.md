---
title:                "Python: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas många olika anledningar till att man vill konvertera en sträng till små bokstäver i sitt Python-program. En vanlig anledning är när man vill jämföra textsträngar utan att skilja på stora och små bokstäver, vilket kan vara användbart när man arbetar med användarinput.

## Så här gör du

För att konvertera en sträng till små bokstäver i Python, kan man använda funktionen "lower()" tillsammans med den aktuella strängen. Här är ett enkelt exempel på hur det kan se ut i kod:

```python
text = "HEJ PÅ DIG"
lowercase_text = text.lower()
print(lowercase_text)
```

Output: hej på dig

Som du ser i exemplet ovan, så har strängen nu konverterats till små bokstäver och har blivit enklare att arbeta med. Denna metod fungerar även för specialtecken och bokstäver med accent.

## Djupdykning

När man konverterar en sträng till små bokstäver, så använder Python sig av Unicode-tabellen för att avgöra vilken bokstav som ska användas. Unicode är ett standardiserat system för att representera bokstäver och tecken från olika språk och skriftsystem.

En intressant sak att notera är att om man använder funktionen "lower()" på en sträng som redan är i små bokstäver, så kommer ingen förändring att ske. Detta på grund av att alla bokstäver i Unicode-tabellen har både en stor och en liten variant, även om de ser likadana ut.

## Se även

- [Python dokumentation: lower() function](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Unicode-table: Basic Latin](https://unicode-table.com/en/blocks/basic-latin/)

Tack för att du läste! Vi hoppas att denna guide har varit till hjälp för dig när du vill konvertera strängar till små bokstäver i Python-program. Lycka till med ditt kodande!