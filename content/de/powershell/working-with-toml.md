---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:24:41.514638-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?

TOML, kurz für Toms Offensichtliche, Minimale Sprache, ist ein Daten-Serialisierungsformat, das leicht zu lesen ist aufgrund seiner klaren Semantik. Programmierer verwenden es für Konfigurationsdateien, da es eine Balance zwischen menschlicher Lesbarkeit und Maschinenfreundlichkeit bietet.

## Wie:

In PowerShell gibt es kein natives Cmdlet, um TOML zu parsen. Normalerweise würden Sie ein Modul verwenden oder TOML mit einem Tool wie `toml-to-json` in JSON konvertieren, wenn Sie mit PowerShell arbeiten möchten. So würden Sie es mit einem fiktiven Modul `PowerShellTOML` machen:

```PowerShell
# Zuerst das Modul installieren (imaginär, zur Demonstration)
Install-Module PowerShellTOML

# Eine TOML-Datei importieren
$config = Import-TomlConfig -Path './config.toml'

# Auf einen Wert zugreifen
Write-Output $config.database.server

# Beispielinhalt von TOML in 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Beispielausgabe:
# 192.168.1.1
```

## Tiefergehend

TOML wurde von Tom Preston-Werner, dem Mitgründer von GitHub, als eine einfachere Alternative zu XML und YAML für Konfigurationsdateien geschaffen. Seine erste Version erschien 2013. TOML ist vergleichbar mit JSON, wurde jedoch so gestaltet, dass es menschenfreundlicher ist, was es zu einer guten Wahl für Konfigurationen macht, die von Menschen gewartet werden. Alternativen umfassen YAML, JSON und XML.

In Bezug auf die Implementierung wäre ein PowerShell-Modul für TOML in der Regel eine Umhüllung um eine TOML-Bibliothek, die in einer leistungsorientierteren Sprache wie C# geschrieben ist. PowerShell hat keine integrierte Unterstützung für TOML, weshalb ein solches Modul notwendig ist, um bequem mit dem TOML-Format zu interagieren.

## Siehe Auch

- TOML-Standard: https://toml.io/en/
- GitHub-Repository für das `toml` PowerShell-Modul (falls zum Zeitpunkt des Lesens vorhanden): https://github.com/powershell/PowerShellTOML
- Eine Einführung in TOML: https://github.com/toml-lang/toml
- Vergleich von Daten-Serialisierungsformaten: https://de.wikipedia.org/wiki/Vergleich_von_Datenserialisierungsformaten
