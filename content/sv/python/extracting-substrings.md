---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar är att plocka ut en specifik del av en sträng i Python. Detta görs ofta för att skilja ut specifik information från en större datauppsättning, ett vanligt behov i datamanipulation och analys.

## Så här gör du:
Du kan extrahera substrings i Python med hjälp av indexering och skivning. Här är ett snabbt exempel:

```Python
text = "Programmera i Python"
print(text[0:11])  # extraherar "Programmera"
```
Output: `Programmera`

Du kan också använda `find()` och `slice()` funktioner:

```Python
start = text.find("Programmera")
end = start + len("Programmera")
print(text[start:end])
```
Output: `Programmera`

## Djupdykning
Extrahering av substrings började användas i tidiga programmeringsspråk som COBOL och Fortran. Alternativa metoder till att extrahera substrings i Python kan vara att använda regular expressions (`re` biblioteket) som kan ge större flexibilitet men också kan bli mer komplicerade. När det gäller prestanda, är den inbyggda slice-funktionen i Python vanligtvis det snabbaste sättet att extrahera substrings, speciellt för stora strängar.

## Se också
Du kan fördjupa dina kunskaper om strängmanipulation i Python via följande resurser:

1. [Officiell Python dokumentation om strängar](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. [Real Python guide till python strängar](https://realpython.com/python-strings/)
3. [W3Schools Python strängar tutorial](https://www.w3schools.com/python/python_strings.asp)