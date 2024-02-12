---
title:                "Arbeiten mit YAML"
aliases:
- /de/powershell/working-with-yaml/
date:                  2024-02-03T19:26:08.915677-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
YAML, oder "YAML Ain't Markup Language", ist eine für Menschen lesbare Datenserialisierungssprache. Programmierer verwenden sie oft für Konfigurationsdateien und den Datenaustausch zwischen Sprachen. Ihre Einfachheit und Lesbarkeit machen sie besonders beliebt für Aufgaben, die das Einrichten von Umgebungen, Anwendungen oder Diensten betreffen, bei denen Konfigurationen entscheidend sind und leicht verstanden und bearbeitet werden sollten.

## Wie:
PowerShell kommt standardmäßig nicht mit einem eingebauten Cmdlet für das Parsen von YAML, aber es funktioniert nahtlos mit YAML, wenn Sie das `powershell-yaml` Modul nutzen oder YAML in ein PowerShell-Objekt umwandeln, indem Sie `ConvertFrom-Json` in Kombination mit einem Tool wie `yq` verwenden.

### Verwendung des `powershell-yaml` Moduls:
Zuerst installieren Sie das Modul:
```PowerShell
Install-Module -Name powershell-yaml
```

Um eine YAML-Datei zu lesen:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Um ein PowerShell-Objekt in eine YAML-Datei zu schreiben:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Beispiel `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Parsen von YAML mit `yq` und `ConvertFrom-Json`:
Ein anderer Ansatz beinhaltet die Verwendung von `yq`, einem leichten und tragbaren Kommandozeilen-YAML-Prozessor. `yq` kann YAML in JSON umwandeln, welches PowerShell nativ parsen kann.

Zuerst stellen Sie sicher, dass `yq` auf Ihrem System installiert ist.
Dann führen Sie aus:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Diese Methode ist besonders nützlich für Benutzer, die in plattformübergreifenden Umgebungen arbeiten oder es vorziehen, JSON innerhalb von PowerShell zu verwenden.
