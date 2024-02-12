---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/python/printing-debug-output.md
date:                  2024-01-20T17:53:21.492516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Å skrive ut debug-informasjon betyr å vise data som hjelper deg å forstå hva programmet ditt gjør. Programmerere gjør dette for å feilsøke kode og sikre at alt kjører som det skal.

## How to:
For å printe debug-informasjon kan du bruke `print()` funksjonen. Legg til `print()` statements der du trenger innsikt.

```python
# et enkelt eksempel
variabel = "Hei, Norge!"
print(variabel)  # Skriver ut variabelens innhold

# finne feil i en løkke
for i in range(5):
    print(f"Verdien av i er nå: {i}")  # Viser hver iterasjon
```

Forventet output:
```
Hei, Norge!
Verdien av i er nå: 0
Verdien av i er nå: 1
Verdien av i er nå: 2
Verdien av i er nå: 3
Verdien av i er nå: 4
```

## Deep Dive
Historisk sett har `print()` alltid vært en enkel måte å undersøke variabler eller flyten av et program. Men, det finnes mer sofistikerte metoder som logger, som lar deg styre detaljnivået av hva som skrives ut. Python's `logging` modul er et slikt alternativ.

Implementeringsdetaljer: Når du bruker `print()`, går output til `sys.stdout`, som vanligvis er konsollen der scriptet kjøres. `logging` lar deg derimot logge til filer, eksterne systemer og kontrollere alvorlighetsgraden av meldinger.

## See Also:
- Den offisielle Python dokumentasjonen om `print()` funksjonen: https://docs.python.org/3/library/functions.html#print
- En dypdykk i Python's `logging` modul: https://docs.python.org/3/library/logging.html
- Stack Overflow for spørsmål og svar: https://stackoverflow.com/questions/tagged/python
