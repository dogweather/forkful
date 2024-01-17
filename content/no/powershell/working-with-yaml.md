---
title:                "Arbeide med yaml"
html_title:           "PowerShell: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML er et tekstbasert markup språk som brukes til å representere data på en strukturert og lesbar måte. Det er en populær format brukt av programmører for å lagre og utveksle data, spesielt i sammenhenger hvor JSON ikke er det beste alternativet.

## Hvordan:

YAML har et enkelt og intuitivt syntaks som gjør det enkelt å lese og skrive. Her er en kodeblokk som viser hvordan du kan definere en liste med frukter i YAML-format:

```PowerShell
frukter: 
  - eple
  - banan
  - appelsin
```
Output:
```PowerShell
frukter:
  - eple
  - banan
  - appelsin
```

For å få tilgang til dataen i YAML-filen, kan du bruke PowerShell cmdleten `Get-Content` og pipe den til cmdleten `ConvertFrom-Yaml`:

```PowerShell
Get-Content .\frukter.yaml | ConvertFrom-Yaml
```

Dette vil gi følgende output:

```PowerShell
frukter
------
{eple, banan, appelsin}
```

Med PowerShell sin fleksibilitet kan du også enkelt manipulere og arbeide med dataen fra YAML-filen i ditt skript.

## Dykk dypere:

YAML har eksistert siden 2001 og ble utviklet som en enklere og mer menneskelesbar versjon av XML. Det er fortsatt et populært valg for lagring av data, spesielt i konfigurasjonsfiler og prosjektoppskrifter.

Et alternativ til YAML er JSON, som har en lignende syntaks, men er noe mindre lesbar for mennesker. Noen foretrekker også å jobbe med JSON-filer da de er bedre støttet i andre programmeringsspråk.

Når det kommer til implementering i PowerShell, er `ConvertFrom-Yaml` cmdleten tilgjengelig fra PowerShell versjon 5 og høyere. Det finnes også Tredjepartsmoduler, som `PSYaml`, som gir flere funksjoner for å arbeide med YAML-filer.

## Se Også:

For mer informasjon om YAML i PowerShell, kan du lese dokumentasjonen fra Microsoft på [ConvertFrom-Yaml](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-yaml?view=powershell-7) cmdleten. Du kan også sjekke ut [PSYaml](https://github.com/cloudbase/PSYaml) tredjepartsmodulen for å få enda flere muligheter når det kommer til håndtering av YAML-filer i PowerShell.