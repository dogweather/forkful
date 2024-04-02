---
date: 2024-02-03 19:03:26.004811-07:00
description: "YAML, or YAML Ain't Markup Language, is a human-readable data serialization\
  \ language. Programmers often use it for configuration files and data\u2026"
lastmod: '2024-03-13T22:45:00.302722-06:00'
model: gpt-4-0125-preview
summary: "YAML, or YAML Ain't Markup Language, is a human-readable data serialization\
  \ language. Programmers often use it for configuration files and data\u2026"
title: Working with YAML
weight: 41
---

## What & Why?
YAML, or YAML Ain't Markup Language, is a human-readable data serialization language. Programmers often use it for configuration files and data transmission between languages. Its simplicity and readability make it particularly popular for tasks involving setting up environments, applications, or services where configurations are crucial and should be easily understood and edited.

## How to:
PowerShell, by default, doesn’t come with a built-in cmdlet for parsing YAML, but it works seamlessly with YAML when you leverage the `powershell-yaml` module or convert YAML into a PowerShell object using `ConvertFrom-Json` in combination with a tool like `yq`.

### Using `powershell-yaml` Module:
First, install the module:
```PowerShell
Install-Module -Name powershell-yaml
```

To read a YAML file:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

To write a PowerShell object into a YAML file:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Sample `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Parsing YAML with `yq` and `ConvertFrom-Json`:
Another approach involves using `yq`, a lightweight and portable command-line YAML processor. `yq` can convert YAML into JSON, which PowerShell can natively parse.

First, ensure `yq` is installed on your system.
Then run:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

This method is particularly useful for users who work in cross-platform environments or prefer using JSON within PowerShell.
