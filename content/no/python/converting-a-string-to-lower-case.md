---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Python: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Konvertering av en streng til små bokstaver er en vanlig oppgave for programmerere. Dette er for å sikre at all tekst behandles på samme måte, uavhengig av store eller små bokstaver. Dette er spesielt viktig når vi sammenligner eller søker i tekst, da det kan gi mer nøyaktige resultater.

# Slik gjør du det:

```python
# Definer en streng:
streng = 'HEI på DEG'

# Bruk lower() funksjonen for å konvertere til små bokstaver:
endret_streng = streng.lower()

# Skriv ut den endrede strengen:
print(endret_streng)
```
Output:
```
hei på deg
```
# Dypdykk:

Historisk sett har konvertering av tekst til små bokstaver vært en utfordring for datamaskiner, spesielt fordi alfabetet var strengt begrenset til store bokstaver. Men i dag støtter de fleste programmeringsspråk funksjoner for å enkelt konvertere tekst til både store og små bokstaver.

I tillegg til lower() funksjonen, er det også en upper() funksjon for å konvertere til store bokstaver. I tillegg kan man også bruke islower() og isupper() for å sjekke om en streng er i henholdsvis små eller store bokstaver.

# Se også:

- [Dokumentasjon for lower() funksjonen](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Python string metoder](https://www.w3schools.com/python/python_strings_methods.asp)
- [Stack Overflow diskusjon om konvertering av strenger til små bokstaver](https://stackoverflow.com/questions/6797984/how-to-convert-string-to-lowercase-in-python)