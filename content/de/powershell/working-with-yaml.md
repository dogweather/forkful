---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein data-serialization Format, benutzt für Konfig-Dateien und Daten-Austausch. Es ist beliebt wegen seiner Lesbarkeit und Einfachheit. Entwickler nutzen YAML für Konfigurationen in vielen Tools wie Docker, Kubernetes und CI/CD Pipelines.
  
## How to:
PowerShell unterstützt YAML nicht nativ, aber mit dem `powershell-yaml` Modul können wir YAML Daten einfach verarbeiten.

### YAML installieren und laden:
```PowerShell
Install-Module -Name powershell-yaml
Import-Module powershell-yaml
```

### YAML-Datei lesen:
```PowerShell
$content = Get-Content -Path './config.yaml' -Raw
$yamlObject = ConvertFrom-Yaml $content
$yamlObject
```

### YAML-Struktur erstellen und schreiben:
```PowerShell
$person = @{
    name = 'Max'
    alter = 30
    sprachen = @('Deutsch', 'Englisch')
}
$yaml = ConvertTo-Yaml $person
$yaml | Out-File -FilePath './neue-config.yaml'
```

```PowerShell
name: Max
alter: 30
sprachen:
  - Deutsch
  - Englisch
```

## Deep Dive
YAML, kurz für "YAML Ain't Markup Language" (ursprünglich "Yet Another Markup Language"), entstand Anfang der 2000er Jahre als einfachere Alternative zu XML. Es wird oft verwendet, weil es für Menschen gut lesbar ist. Alternative Formate wie JSON werden zwar manchmal für machine-to-machine Kommunikation bevorzugt, aber YAML bleibt König für Konfig-Dateien. PowerShell mit `powershell-yaml` macht das Lesen und Schreiben von YAML einfach, ist jedoch eine Community-Lösung und nicht offiziell Teil von PowerShell.

## See Also
Weitere Infos und Hilfe zu YAML in PowerShell:

- powershell-yaml Modul: [https://github.com/cloudbase/powershell-yaml](https://github.com/cloudbase/powershell-yaml)
- YAML offizielle Seite: [https://yaml.org/](https://yaml.org/)
- YAML Syntax: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
