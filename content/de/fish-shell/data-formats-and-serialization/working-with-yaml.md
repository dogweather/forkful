---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:51.884164-07:00
description: "Die Arbeit mit YAML umfasst das Parsen und Manipulieren von YAML-Dateien\
  \ (YAML Ain't Markup Language), einem Daten-Serialisierungsformat, das f\xFCr\u2026"
lastmod: '2024-02-25T18:49:51.379473-07:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit YAML umfasst das Parsen und Manipulieren von YAML-Dateien\
  \ (YAML Ain't Markup Language), einem Daten-Serialisierungsformat, das f\xFCr\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit YAML umfasst das Parsen und Manipulieren von YAML-Dateien (YAML Ain't Markup Language), einem Daten-Serialisierungsformat, das für Konfigurationsdateien verwendet wird, in der Fish Shell. Programmierer tun dies, um Anwendungen oder Dienste effizient innerhalb des Kontexts von Shell-Umgebungen zu automatisieren und zu konfigurieren, wodurch Aufgaben wie die Verwaltung von Konfigurationen und das Bereitstellen von Ressourcen erleichtert werden.

## Wie:
Fish Shell hat keine eingebaute Unterstützung zum Parsen von YAML, aber man kann Drittanbieter-Tools wie `yq` (einen leichtgewichtigen und tragbaren Befehlszeilen-YAML-Prozessor) nutzen, um YAML-Daten zu verarbeiten.

**Installation von yq (falls nicht bereits installiert):**
```fish
sudo apt-get install yq
```

**Lesen eines Wertes aus einer YAML-Datei:**
Angenommen, Sie haben eine YAML-Datei `config.yaml` mit folgendem Inhalt:
```yaml
database:
  host: localhost
  port: 3306
```

Um den Datenbank-Host zu lesen, würden Sie verwenden:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Beispielausgabe:**
```
localhost
```

**Aktualisieren eines Wertes in einer YAML-Datei:**
Um den `port` auf `5432` zu aktualisieren, verwenden Sie:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Überprüfen der Aktualisierung:**
```fish
yq e '.database.port' config.yaml
```
**Beispielausgabe:**
```
5432
```

**Erstellen einer neuen YAML-Datei:**
Um eine neue `new_config.yaml` mit vordefiniertem Inhalt zu erstellen:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Dies verwendet `yq`, um einen String zu verarbeiten und hübsch zu drucken (-P Flag) in eine neue YAML-Datei.

**Parsen komplexer Strukturen:**
Wenn Sie eine komplexere YAML-Datei haben und verschachtelte Arrays oder Objekte abrufen müssen, können Sie:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Beispielausgabe:**
```
server1
server2
```
Mit `yq` macht Fish Shell es unkompliziert, durch YAML-Dokumente zu navigieren und sie für verschiedene Automatisierungs- und Konfigurationsaufgaben zu manipulieren.
