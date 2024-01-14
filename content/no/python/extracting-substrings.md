---
title:    "Python: Ekstrahering av substringer"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substringer er en viktig ferdighet når du jobber med tekstbehandling i Python. Dette kan være nyttig for å finne og manipulere spesifikke deler av en tekst, for eksempel å finne et bestemt ord eller tall.

## Hvordan

For å trekke ut en substring i Python, kan du bruke indeksering og slicing. Dette betyr at du velger ut en del av en tekst ved å indikere start- og sluttpunktet.

Her er et eksempel på hvordan du kan trekke ut substrings fra en tekst og skrive ut dem:

```Python
tekst = "Dette er en tekst for å demonstrere substrings"

# Trekke ut og skrive ut "tekst"
substring1 = tekst[11:15]
print(substring1)
# Output: tekst

# Trekke ut og skrive ut "dem"
substring2 = tekst[39:42]
print(substring2)
# Output: dem

# Trekke ut og skrive ut "demonstrere substrings"
substring3 = tekst[-28:]
print(substring3)
# Output: demonstrere substrings
```

Man kan også bruke metoden `.split()` for å trekke ut substrings basert på et gitt skille. Dette er nyttig hvis du for eksempel har en tekst med ord delt av mellomrom og du vil trekke ut hvert ord som en substring.

Her er et eksempel på hvordan du kan bruke `.split()` og skrive ut hver substring som en liste:

```Python
tekst = "Dette er en tekst for å demonstrere substrings"

# Trekke ut og skrive ut hvert ord som en substring
substring_liste = tekst.split(" ")
print(substring_liste)
# Output: ['Dette', 'er', 'en', 'tekst', 'for', 'å', 'demonstrere', 'substrings']
```

## Dypdykk

Når man arbeider med trekke ut substrings i Python, er det viktig å forstå hvordan indeksering fungerer. Indeksene starter alltid på 0 og går bakoversveis, dermed vil den siste bokstaven i en tekst ha indeks -1.

Det finnes også flere metoder for å trekke ut substrings, som for eksempel `.find()` og `.index()`. Disse metodene lar deg finne indeksen til en bestemt del av teksten basert på et gitt søkeord eller tegn.

## Se også

- [Python string methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [String handling in Python](https://realpython.com/python-strings/)
- [How to work with strings in Python](https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3)