---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:19.502778-07:00
description: "Das Abrufen des aktuellen Datums in der Programmierung ist eine grundlegende\
  \ Aufgabe, die es Ihnen erm\xF6glicht, das Datum und die Zeitdaten des Systems\u2026"
lastmod: '2024-02-25T18:49:51.370054-07:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in der Programmierung ist eine grundlegende\
  \ Aufgabe, die es Ihnen erm\xF6glicht, das Datum und die Zeitdaten des Systems\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in der Programmierung ist eine grundlegende Aufgabe, die es Ihnen ermöglicht, das Datum und die Zeitdaten des Systems abzurufen und zu manipulieren. Bei Skripten und Automatisierungsaufgaben ist es unerlässlich für die Generierung von Zeitstempeln, das Planen von Aufgaben und das Erstellen von Protokollen.

## Wie geht das:
Fish Shell verwendet externe Befehle wie `date`, um das aktuelle Datum zu erhalten, und bietet Flexibilität, das Ausgabeformat nach Bedarf zu formatieren. So verwenden Sie es:

```fish
# Das aktuelle Datum im Standardformat anzeigen
echo (date)

# Ausgabebeispiel: Wed 25 Oct 2023 15:42:03 BST
```

Um das Format des Datums anzupassen, können Sie die Option `+` gefolgt von Formatspezifikatoren verwenden:

```fish
# Das aktuelle Datum im YYYY-MM-DD-Format anzeigen
echo (date "+%Y-%m-%d")

# Ausgabebeispiel: 2023-10-25
```

Für komplexere Aufgaben, wie das Arbeiten mit Zeitstempeln oder das Ausführen von Datumsrechnungen, verlässt sich Fish Shell auf externe Werkzeuge wie `date` wegen seiner Skriptnatur. Hier ist ein Beispiel dafür, wie der aktuelle UNIX-Zeitstempel abgerufen wird:

```fish
# Den aktuellen UNIX-Zeitstempel abrufen
echo (date "+%s")

# Ausgabebeispiel: 1666710123
```

Und um einen Tag zum aktuellen Datum hinzuzufügen mit `date`:

```fish
# Einen Tag zum aktuellen Datum hinzufügen
echo (date -d "+1 day" "+%Y-%m-%d")

# Ausgabebeispiel: 2023-10-26
```

Hinweis: Die Beispiele verwenden die `date`-Befehlsoptionen, die mit den GNU-Coreutils funktionieren. Optionen können sich in anderen Umgebungen wie macOS, das standardmäßig den BSD-Date-Befehl verwendet, unterscheiden. Beziehen Sie sich immer auf `date --help` oder die Man-Seite für Details, die spezifisch für Ihre Umgebung sind.
