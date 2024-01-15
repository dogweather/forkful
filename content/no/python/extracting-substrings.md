---
title:                "Ekstrahering av delstrenger"
html_title:           "Python: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil noen ønske å eksrakt substringer? Ved å eksrakte deler av en tekststreng, kan man få tilgang til spesifikke deler av informasjonen som kan være relevant for videre behandling eller analyse.

## Hvordan

Å eksrakte substringer i Python er en enkel prosess. Først må vi definere tekststrengen vi ønsker å ekstrahere fra. La oss si at vi har følgende tekst: "Hei, jeg heter Maria". 

```Python
tekststreng = "Hei, jeg heter Maria"

# Eksrakter første del av tekststrengen (fra start til index 10)
print(tekststreng[:10])
# Output: Hei, jeg h

# Eksrakter delen etter første komma
print(tekststreng.split(",")[1])
# Output:  jeg heter Maria
```

I det første eksempelet brukte vi en *slice* operasjon for å ekstrahere de første 10 karakterene i tekststrengen. Dette gjør vi ved å bruke `[start:stop]` notasjon, der `start` er inkludert og `stop` er ekskludert. I det andre eksempelet brukte vi `split()` funksjonen for å dele tekststrengen ved hjelp av komma som skille-tegn, og deretter hente ut den andre delen av listen.

## Dypdykk

Det finnes flere metoder for å ekstrahere substringer i Python. Her er noen eksempler:

- `rfind(substring)` - returnerer start indeksen til den siste forekomsten av `substring` i tekststrengen.
- `casefold()` - konverterer tekststrengen til lowercase.
- `replace(old_substring, new_substring)` - erstatter alle forekomster av `old_substring` med `new_substring`.
- `strip(characters)` - fjerner alle spesifiserte `characters` fra starten og slutten av tekststrengen.

Et tips: når du jobber med tekststrenger, kan det være nyttig å bruke `format()` metoden for å enklere manipulere og sette sammen tekster.

## Se også

- [Python's string methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Practice with string exercises](https://pynative.com/python-string-exercise/)
- [The Power of Python's string.format](https://dbader.org/blog/python-string-formatting)