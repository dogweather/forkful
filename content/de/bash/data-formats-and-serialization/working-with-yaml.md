---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:53.284339-07:00
description: "Wie: Direkt mit YAML in Bash zu arbeiten, erfordert ein wenig Einfallsreichtum,\
  \ da Bash keine integrierte Unterst\xFCtzung f\xFCr das Parsen von YAML bietet.\u2026"
lastmod: '2024-03-13T22:44:54.079925-06:00'
model: gpt-4-0125-preview
summary: "Direkt mit YAML in Bash zu arbeiten, erfordert ein wenig Einfallsreichtum,\
  \ da Bash keine integrierte Unterst\xFCtzung f\xFCr das Parsen von YAML bietet."
title: Arbeiten mit YAML
weight: 41
---

## Wie:
Direkt mit YAML in Bash zu arbeiten, erfordert ein wenig Einfallsreichtum, da Bash keine integrierte Unterstützung für das Parsen von YAML bietet. Sie können jedoch externe Tools wie `yq` (einen leichten und tragbaren Kommandozeilen YAML-Prozessor) nutzen, um effizient mit YAML-Dateien zu interagieren. Gehen wir einige gängige Operationen durch:

### `yq` installieren:
Bevor Sie in die Beispiele eintauchen, stellen Sie sicher, dass Sie `yq` installiert haben. Sie können es normalerweise über Ihren Paketmanager installieren, zum Beispiel auf Ubuntu:

```bash
sudo apt-get install yq
```

Oder Sie können es direkt aus seinem GitHub-Repository herunterladen.

### Einen Wert lesen:
Angenommen, Sie haben eine Datei namens `config.yaml` mit dem folgenden Inhalt:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: geheim
```

Um den Datenbankhost zu lesen, können Sie `yq` wie folgt verwenden:

```bash
yq e '.database.host' config.yaml
```

**Beispielausgabe:**

```
localhost
```

### Einen Wert aktualisieren:
Um den Namen des Benutzers in `config.yaml` zu aktualisieren, verwenden Sie den Befehl `yq eval` mit der Option `-i` (in-place):

```bash
yq e '.user.name = "neuadmin"' -i config.yaml
```

Überprüfen Sie die Änderung mit:

```bash
yq e '.user.name' config.yaml
```

**Beispielausgabe:**

```
neuadmin
```

### Ein neues Element hinzufügen:
Um ein neues Element im Datenbankabschnitt hinzuzufügen, wie ein neues Feld `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Das Überprüfen des Inhalts der Datei wird die Ergänzung bestätigen.

### Ein Element löschen:
Um das Passwort unter Benutzer zu entfernen:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Diese Operation wird das Passwortfeld aus der Konfiguration entfernen.

Denken Sie daran, `yq` ist ein leistungsfähiges Werkzeug und bietet noch viele weitere Möglichkeiten, einschließlich der Konvertierung von YAML zu JSON, dem Zusammenführen von Dateien und sogar komplexeren Manipulationen. Schauen Sie in die `yq`-Dokumentation für weitere Erkundungen.
