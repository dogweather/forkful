---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:50.051203-07:00
description: "YAML is een gebruiksvriendelijk data serialisatieformaat. Programmeurs\
  \ gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen en\u2026"
lastmod: '2024-03-13T22:44:51.050528-06:00'
model: gpt-4-0125-preview
summary: YAML is een gebruiksvriendelijk data serialisatieformaat.
title: Werken met YAML
weight: 41
---

## Hoe te:
Om met YAML in PowerShell te werken, moet je een module zoals `powershell-yaml` gebruiken. Installeer het eerst:

```PowerShell
Install-Module -Name powershell-yaml
```

YAML-inhoud lezen:

```PowerShell
# Importeer de module
Import-Module powershell-yaml

# Laad een YAML-bestand
$yamlContent = Get-Content -Path 'config.yaml' -Raw

# Zet YAML om naar een PowerShell-object
$configObject = ConvertFrom-Yaml -Yaml $yamlContent

# Output het object
$configObject
```

YAML maken en schrijven:

```PowerShell
# Maak een hashtable
$person = @{
  name = 'Jane Doe'
  age = 30
  languages = @('Engels', 'Frans')
}

# Zet de hashtable om naar YAML
$yamlOutput = ConvertTo-Yaml -Data $person

# Schrijf de YAML naar een bestand
$yamlOutput | Out-File -FilePath 'person.yaml'
```

## Diepgaand
YAML vindt zijn oorsprong in het begin van de jaren 2000 en staat voor "YAML Ain't Markup Language", een recursief acroniem dat de nadruk legt op zijn datagerichte benadering in tegenstelling tot opmaaktalen zoals HTML. Hoewel JSON vaak de voorkeurskeuze is voor API's en webservices vanwege de efficiënte parsing en compactheid, blijft YAML populair vanwege de leesbaarheid en het feit dat het meer menselijk bewerkbaar is, vooral in configuratiebestanden (bijv. Docker Compose en Kubernetes).

Alternatieven voor `powershell-yaml` zijn onder meer `YamlDotNet` met `.NET` lijmcode, of het handmatig parseren van YAML strings - maar waarom zou je je leven compliceren?

Onder de motorkap gebruikt `powershell-yaml` `YamlDotNet`, waarbij YAML omgezet wordt naar .NET-objecten die PowerShell gemakkelijk kan afhandelen. Deze interoperabiliteit maakt een soepele overgang van YAML-gegevens naar het PowerShell-ecosysteem mogelijk.

## Zie Ook
- [`powershell-yaml` op PowerShell Galerij](https://www.powershellgallery.com/packages/powershell-yaml)
- [YAML Officiële Website](https://yaml.org/)
- [YAML Syntax Referentie](https://learnxinyminutes.com/docs/yaml/)
