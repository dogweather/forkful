---
title:                "Arbeta med yaml"
html_title:           "PowerShell: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML står för "YAML Ain't Markup Language" (på svenska "YAML är ingen markup-språk") och det är ett lättläst format för data. YAML används ofta av programmerare för att konfigurera och strukturera data i sina applikationer.

## Hur man:
För att arbeta med YAML i PowerShell kan du använda modulen PSYaml. Här är ett exempel på hur du kan skapa en YAML-fil och sedan läsa in den i PowerShell:

```PowerShell
# Skapa en YAML-fil
@"
---
Person:
  Namn: John Smith
  Ålder: 35
  Yrke: Programmerare
  Språk:
    - PowerShell
    - C#
    - Python
"@
| Out-File -FilePath '.\person.yaml'

# Läs in filen och konvertera till PowerShell-objekt
$person = Get-Content -Path '.\person.yaml' | ConvertFrom-Yaml

# Visa information om personen
Write-Output "Namn: $($person.Person.Namn)"
Write-Output "Ålder: $($person.Person.Ålder)"
Write-Output "Yrke: $($person.Person.Yrke)"
Write-Output "Språk: $($person.Person.Språk -join ', ')"
```

Output:
```
Namn: John Smith
Ålder: 35
Yrke: Programmerare
Språk: PowerShell, C#, Python
```

## Fördjupning
YAML skapades ursprungligen av Ingy döt Net och Clark Evans 2001. Det är ett enkelt och intuitivt alternativ till JSON och XML för att strukturera data.

En annan fördel med YAML är att det är människoläsbar, vilket gör det lättare att felsöka och förstå.

Förutom den tidigare nämnda PSYaml-modulen finns det också andra möjligheter att arbeta med YAML i PowerShell, som t.ex. YAMLDotNet-modulen.

Vid implementering är det viktigt att vara noggrann med indrag och använda korrekt syntax för att undvika fel.

## Se även
- [PSYaml-modulen på PowerShell Gallery](https://www.powershellgallery.com/packages/PSYaml)
- [YAML-specifikationen](https://yaml.org/spec/1.2/spec.html)
- [YAMLDotNet-modulen på PowerShell Gallery](https://www.powershellgallery.com/packages/YamlDotNet)