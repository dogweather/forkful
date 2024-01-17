---
title:                "Store bokstaver i en streng"
html_title:           "PowerShell: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

** Hva og hvorfor? **

Capitalizing en streng betyr å endre den første bokstaven til store bokstaver. Programmere gjør dette for å gjøre tekst lettere å lese og for å følge standarder og konvensjoner i koden deres.

** Hvordan: **

``` PowerShell
# Eksempel:
$streng = "hei, dette er en tekststreng."

# Bruk .ToUpper() metoden:
$streng.ToUpper()

# Output:
HEI, DETTE ER EN TEKSTSTRENG.
```

** Dypdykk: **

1. Historisk sammenheng: Capitalization har vært brukt i språk i mange århundrer for å skille mellom forskjellige typer ord.
2. Alternativer: I tillegg til .ToUpper() metoden, kan du også bruke .ToUpperInvariant() for å sikre at store bokstaver blir brukt uavhengig av språkinnstillinger. Du kan også bruke .ToTitleCase() metoden for å forbedre leseligheten og følge riktig hovedstad konvensjoner.
3. Implementeringsdetaljer: I PowerShell brukes .ToUpper() metoden for å konvertere en streng til store bokstaver. Dette kan også brukes på individuelle tegn eller en del av en streng.

** Se også: **

- Dokumentasjon for .ToUpper() metoden: https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netframework-4.8
- PowerShell stilenhetsstandarder: https://github.com/PoshCode/PowerShellPracticeAndStyle