---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Å gjøre en streng stor, eller "capitalize", betyr å gjøre det første bokstavet i en streng til en stor bokstav. Programmerere gjør dette for å formatere tekst for lesbarhet, som når man skal starte setninger eller gjøre navn konsekvente.

## How to:
Python gjør det lett å stor-bokstavere strenger. Her er noen eksempler:

```python
# Bruk capitalize() for å stor-bokstavere den første bokstaven i en streng
tekst = "norge"
kapitalisert_tekst = tekst.capitalize()
print(kapitalisert_tekst)  # Output: Norge

# Bruk title() for å stor-bokstavere den første bokstaven i hvert ord
tittel = "python programmering"
formell_tittel = tittel.title()
print(formell_tittel)  # Output: Python Programmering
```

Kodeblokkene viser enkle måter å gjøre første bokstav eller alle ord startbokstaver storer.

## Deep Dive
Strengkapitalisering har eksistert like lenge som programmeringsspråk har hatt behov for å manipulere tekst. Python har alltid gjort det enkelt å jobbe med tekststrenger.

Historisk sett kommer behovet for kapitalisering fra ønsket om standardisert formatering, som i bøker og dokumenter. I tidlige dagers databehandling var behandling av tekst en viktig del av programvareutvikling, og dette inkluderte å kunne kontrollere hvordan bokstaver ble presentert.

I Python har `.capitalize()` og `.title()` metoder vært de konvensjonelle måtene å kapitalisere strenger. Det finnes alternativer, som:

- Bruke `.upper()` for å gjøre alle bokstaver til store bokstaver.
- Bruke `.lower()` etterfulgt av indexering og konkatinasjon for å selv skape en capitalize-funksjon.
- Regex (regular expression) for mer kompleks tekstmanipulasjon som kan inkludere kapitalisering basert på mønstre.

Kapitalisering i Python utføres på Unicode-strenger, noe som betyr at den tar høyde for internasjonale skriftsystemer og språk-spesifikke bokstaver.

## See Also
Her er noen nyttige lenker:

- Python String Methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- Unicode in Python: https://docs.python.org/3/howto/unicode.html
- Regular Expressions in Python: https://docs.python.org/3/library/re.html

Disse lenkene gir en grundig forklaring av strengmetoder, håndtering av Unicode i Python, og bruk av regex for tekstbehandling.
