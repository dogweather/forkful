---
title:                "Lavorare con YAML"
aliases:
- it/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:11.410012-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML, o YAML Ain't Markup Language, è un linguaggio di serializzazione dei dati leggibile dall'uomo. Gli sviluppatori lo usano spesso per file di configurazione e trasmissione di dati tra linguaggi. La sua semplicità e leggibilità lo rendono particolarmente popolare per compiti che coinvolgono la configurazione di ambienti, applicazioni o servizi dove le configurazioni sono cruciali e dovrebbero essere facilmente comprensibili e modificabili.

## Come fare:
PowerShell, di default, non viene fornito con un cmdlet integrato per l'analisi di YAML, ma funziona senza problemi con YAML quando si sfrutta il modulo `powershell-yaml` o si converte YAML in un oggetto PowerShell usando `ConvertFrom-Json` in combinazione con uno strumento come `yq`.

### Utilizzando il modulo `powershell-yaml`:
Prima, installa il modulo:
```PowerShell
Install-Module -Name powershell-yaml
```

Per leggere un file YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Per scrivere un oggetto PowerShell in un file YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Esempio di `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Analizzare YAML con `yq` e `ConvertFrom-Json`:
Un altro approccio coinvolge l'uso di `yq`, un processore YAML da linea di comando leggero e portatile. `yq` può convertire YAML in JSON, che PowerShell può analizzare nativamente.

Prima, assicurati che `yq` sia installato sul tuo sistema.
Poi esegui:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Questo metodo è particolarmente utile per gli utenti che lavorano in ambienti multipiattaforma o preferiscono usare JSON all'interno di PowerShell.
