---
date: 2024-01-26 04:24:41.514638-07:00
description: "TOML, kurz f\xFCr Toms Offensichtliche, Minimale Sprache, ist ein Daten-Serialisierungsformat,\
  \ das leicht zu lesen ist aufgrund seiner klaren Semantik.\u2026"
lastmod: '2024-03-13T22:44:54.126899-06:00'
model: gpt-4-0125-preview
summary: "TOML, kurz f\xFCr Toms Offensichtliche, Minimale Sprache, ist ein Daten-Serialisierungsformat,\
  \ das leicht zu lesen ist aufgrund seiner klaren Semantik."
title: Arbeiten mit TOML
weight: 39
---

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
