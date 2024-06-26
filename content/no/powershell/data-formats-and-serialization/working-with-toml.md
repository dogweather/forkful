---
date: 2024-01-26 04:25:01.192071-07:00
description: "Hvordan: I PowerShell finnes det ikke noe innebygd cmdlet for \xE5 tolke\
  \ TOML. Du ville typisk brukt en modul eller konvertert TOML til JSON med et verkt\xF8\
  y\u2026"
lastmod: '2024-03-13T22:44:41.040314-06:00'
model: gpt-4-0125-preview
summary: "I PowerShell finnes det ikke noe innebygd cmdlet for \xE5 tolke TOML."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
I PowerShell finnes det ikke noe innebygd cmdlet for å tolke TOML. Du ville typisk brukt en modul eller konvertert TOML til JSON med et verktøy som `toml-to-json` hvis du ønsker å jobbe med PowerShell. Slik gjør du det med en fiktiv modul `PowerShellTOML`:

```PowerShell
# Først, installer modulen (imaginær, for demonstrasjon)
Install-Module PowerShellTOML

# Importer en TOML-fil
$config = Import-TomlConfig -Path './config.toml'

# Tilgang til en verdi
Write-Output $config.database.server

# Eksempel på TOML-innhold i 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Eksempel på utdata:
# 192.168.1.1
```

## Dypdykk
TOML ble skapt av Tom Preston-Werner, medstifter av GitHub, som et enklere alternativ til XML og YAML for konfigurasjonsfiler. Den første versjonen dukket opp i 2013. TOML kan sammenlignes med JSON, men er designet for å være mer menneskevennlig, noe som gjør det til et godt valg for konfigurasjon som opprettholdes av mennesker. Alternativene inkluderer YAML, JSON, og XML.

Når det gjelder implementasjon, vil en PowerShell-modul for TOML typisk være et wrapper rundt et TOML-bibliotek skrevet i et mer ytelsesorientert språk som C#. PowerShell har ikke innebygd støtte for TOML, derfor er en slik modul nødvendig for å samhandle med TOML-formatet på en praktisk måte.

## Se Også
- TOML-standard: https://toml.io/en/
- GitHub-repositorium for `toml` PowerShell-modulen (hvis det eksisterer på lesetidspunktet): https://github.com/powershell/PowerShellTOML
- En introduksjon til TOML: https://github.com/toml-lang/toml
- Sammenligning av data serialiseringsformater: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
