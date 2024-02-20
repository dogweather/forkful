---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:16.339995-07:00
description: "YAML, eller YAML Ain't Markup Language, \xE4r ett m\xE4nniskol\xE4sbart\
  \ dataserieringsspr\xE5k. Programmerare anv\xE4nder det ofta f\xF6r konfigurationsfiler\
  \ och\u2026"
lastmod: 2024-02-19 22:04:57.380082
model: gpt-4-0125-preview
summary: "YAML, eller YAML Ain't Markup Language, \xE4r ett m\xE4nniskol\xE4sbart\
  \ dataserieringsspr\xE5k. Programmerare anv\xE4nder det ofta f\xF6r konfigurationsfiler\
  \ och\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, eller YAML Ain't Markup Language, är ett människoläsbart dataserieringsspråk. Programmerare använder det ofta för konfigurationsfiler och dataöverföring mellan språk. Dess enkelhet och läsbarhet gör det särskilt populärt för uppgifter som involverar att sätta upp miljöer, applikationer eller tjänster där konfigurationer är avgörande och bör vara lättförståeliga och redigerbara.

## Hur:
PowerShell kommer som standard inte med en inbyggd cmdlet för att tolka YAML, men det fungerar sömlöst med YAML när du använder `powershell-yaml`-modulen eller konverterar YAML till ett PowerShell-objekt med `ConvertFrom-Json` i kombination med ett verktyg som `yq`.

### Använda `powershell-yaml`-modulen:
Först, installera modulen:
```PowerShell
Install-Module -Name powershell-yaml
```

För att läsa en YAML-fil:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

För att skriva ett PowerShell-objekt till en YAML-fil:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Exempel `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Tolka YAML med `yq` och `ConvertFrom-Json`:
En annan metod innebär att använda `yq`, en lättviktig och portabel kommandorads-YAML-processor. `yq` kan konvertera YAML till JSON, som PowerShell kan tolka inbyggt.

Först, se till att `yq` är installerat på ditt system.
Kör sedan:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Denna metod är särskilt användbar för användare som arbetar i plattformsoberoende miljöer eller föredrar att använda JSON inom PowerShell.
