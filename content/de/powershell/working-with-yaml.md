---
title:                "Arbeiten mit YAML"
html_title:           "PowerShell: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

Was ist YAML und warum verwenden Programmierer es?

YAML ist eine textbasierte Datenformatierungssprache, die oft von Programmierern verwendet wird, um strukturierte Daten zu speichern. YAML steht für "YAML Ain't Markup Language" und wurde entwickelt, um eine einfache und benutzerfreundliche Alternative zu anderen Datenformatierungssprachen wie XML zu bieten. Programmierer verwenden YAML, um Daten zu speichern, zu übertragen und zu verarbeiten, insbesondere in Anwendungen wie Webentwicklung, Datenbankmanagement und Softwarekonfiguration.

Wie geht's:

```PowerShell
# Hier ein kleines Beispiel, wie YAML in PowerShell verwendet werden kann:
$yaml = @"
name: John
age: 30
hobbies:
  - coding
  - gaming
  - hiking
"@

# YAML in PowerShell umwandeln:
$object = (ConvertFrom-Yaml $yaml)
# Der Inhalt von $object wird nun als Objekt mit Eigenschaften (name, age, hobbies) gespeichert

# Oder umgekehrt, ein PowerShell-Objekt in YAML umwandeln:
$object.name = "Sarah"
$yaml = $obj | ConvertTo-Yaml
# Der Inhalt von $yaml wird nun als formatiertes YAML-Dokument gespeichert, in dem "Sarah" als Wert für die Eigenschaft "name" angegeben ist.

```

In die Tiefe:

- Historischer Kontext: YAML wurde 2001 von Clark Evans entwickelt und ist seitdem kontinuierlich weiterentwickelt worden. Es ist Teil der YAML-Sprachfamilie, die auch YAML-Dateien umfasst.
- Alternativen: Wie bereits erwähnt, gibt es andere Datenformatierungssprachen wie XML und JSON, die ähnliche Zwecke erfüllen können. Die Wahl der richtigen Sprache hängt von den Anforderungen des Projekts und den Vorlieben der Entwickler ab.
- Implementierungsdetails: YAML in PowerShell wird durch das Modul "YamlDotNet" ermöglicht, das über den PowerShell Gallery heruntergeladen werden kann. Weitere Informationen und Beispiele zum Umgang mit YAML in PowerShell finden Sie in der offiziellen Dokumentation des Moduls.

Siehe auch:

Weitere Informationen zu YAML in PowerShell und detaillierte Beispiele finden Sie in der offiziellen Dokumentation des "YamlDotNet" Moduls:

https://www.powershellgallery.com/packages/YamlDotNet/

Weitere Ressourcen zur Syntax und Verwendung von YAML:

- Offizielle YAML-Dokumentation: https://yaml.org/
- YAML-Syntaxüberblick: https://yaml.org/spec/1.2/spec.html#id2760395
- Ein interaktiver Lernleitfaden für YAML: https://learnxinyminutes.com/docs/yaml/