---
title:                "Werken met TOML"
aliases:
- nl/powershell/working-with-toml.md
date:                  2024-01-28T22:11:04.622760-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

TOML, kort voor Tom's Obvious, Minimal Language, is een gegevensserialisatieformaat dat gemakkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs gebruiken het voor configuratiebestanden, omdat het een evenwicht biedt tussen leesbaar voor mensen en vriendelijk voor machines.

## Hoe te:

In PowerShell is er geen native cmdlet om TOML te parsen. Typisch zou je een module gebruiken of TOML naar JSON converteren met een tool zoals `toml-to-json` als je met PowerShell wilt werken. Hier is hoe je het zou doen met een fictieve module `PowerShellTOML`:

```PowerShell
# Eerst, installeer de module (denkbeeldig, ter demonstratie)
Install-Module PowerShellTOML

# Importeer een TOML-bestand
$config = Import-TomlConfig -Path './config.toml'

# Een waarde benaderen
Write-Output $config.database.server

# Voorbeeld van TOML inhoud in 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Voorbeelduitvoer:
# 192.168.1.1
```

## Diepere Duik

TOML werd gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, als een eenvoudiger alternatief voor XML en YAML voor configuratiebestanden. De eerste versie verscheen in 2013. TOML is vergelijkbaar met JSON, maar het is ontworpen om vriendelijker te zijn voor mensen, wat het een goede keuze maakt voor configuraties die door mensen worden onderhouden. Alternatieven zijn YAML, JSON en XML.

Wat betreft de implementatie, zou een PowerShell-module voor TOML typisch een wrapper zijn rond een TOML-bibliotheek geschreven in een meer prestatiegerichte taal zoals C#. PowerShell heeft geen ingebouwde ondersteuning voor TOML, wat de reden is waarom zo'n module nodig is om gemakkelijk met het TOML-formaat te kunnen werken.

## Zie Ook

- TOML standaard: https://toml.io/nl/
- GitHub repository voor `toml` PowerShell module (indien deze bestaat op het moment van lezing): https://github.com/powershell/PowerShellTOML
- Een introductie tot TOML: https://github.com/toml-lang/toml
- Vergelijking van gegevensserialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_gegevensserialisatieformaten
