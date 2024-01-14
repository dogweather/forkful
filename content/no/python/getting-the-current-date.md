---
title:                "Python: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å få nåværende dato? Vel, det er flere grunner til hvorfor dette er nyttig når du programmerer. Først og fremst kan det hjelpe med å organisere og sortere data. Det kan også være nyttig å bruke i logger for å spore når en bestemt handling ble utført. Uansett hva grunnen måtte være, er det enkelt å få tak i nåværende dato ved hjelp av Python-programmering.

## Hvordan

For å få tak i nåværende dato i Python, kan du bruke datetime-modulen. Først må du importere denne modulen ved å skrive ```Python import datetime``` øverst i koden din. Deretter kan du bruke datetime.now() funksjonen for å få tak i nåværende dato og tid.

Her er et eksempel på hvordan du kan skrive ut nåværende dato og tid ved hjelp av datetime-modulen:

```Python
import datetime

now = datetime.now()
print("Nåværende dato og tid:")
print(now)
```

Dette vil produsere følgende output:

Nåværende dato og tid:
2021-09-28 15:00:00.123456

Du kan også formatere hvordan datoen og tiden vises ved hjelp av strftime() funksjonen. For eksempel, hvis du vil ha datoen i formatet MM/DD/YYYY, kan du skrive:

```Python
print(now.strftime("%m/%d/%Y"))
```

Dette vil produsere output som ser slik ut:

09/28/2021

## Dypdykk

Det er flere alternativer og funksjoner som kan brukes sammen med datetime-modulen for å få mer spesifikke data som dato, tid og tidsstempel. Du kan også bruke timedelta-modulen for å beregne fremtidige eller tidligere datoer basert på nåværende dato.

Hvis du ønsker å lære mer om hvordan du kan bruke datetime-modulen og andre relaterte emner til dato og tid i Python, er det mange ressurser tilgjengelig online, inkludert dokumentasjon fra Python.org og ulike programmeringsforum.

## Se også

- Python.org - datetime-modulen
- W3Schools - Dato og tid i Python
- Stack Overflow - Spørsmål og svar om datetime-modulen i Python