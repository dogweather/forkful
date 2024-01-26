---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per dati leggibili dall'essere umano, spesso usato per configurare software. I programmatori lo usano per la semplicità e la chiarezza della sua sintassi.

## How to:
Installiamo il modulo PowerShell `powershell-yaml` con:

```PowerShell
Install-Module -Name powershell-yaml
```

Leggiamo un file YAML con:

```PowerShell
Import-Module powershell-yaml
$yamlContent = Get-Content -Path 'config.yml' | ConvertFrom-Yaml
$yamlContent
```

Questo è un esempio di output:

```yaml
name: Giovanni
age: 30
languages:
  - Italiano
  - Inglese
```

Per creare un nuovo file YAML usiamo:

```PowerShell
$person = @{
  name = 'Giovanni'
  age = 30
  languages = @('Italiano', 'Inglese')
}

$person | ConvertTo-Yaml | Out-File -FilePath 'newConfig.yml'
```

## Deep Dive
YAML è nato nel 2001 con l'obiettivo di essere più semplice dell'XML. Ci sono alternative come JSON, ma YAML è preferito per la sua leggibilità. YAML si basa sull'indentazione per definire la struttura, il che lo rende sensibile agli spazi, ma anche intuitivo nell'uso.

## See Also
- Documentazione ufficiale di YAML: https://yaml.org/spec/1.2/spec.html
- PowerShell Gallery per il modulo `powershell-yaml`: https://www.powershellgallery.com/packages/powershell-yaml
- YAML Lint per validare il tuo YAML: http://www.yamllint.com/
