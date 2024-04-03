---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:19.502778-07:00
description: "Wie geht das: Fish Shell verwendet externe Befehle wie `date`, um das\
  \ aktuelle Datum zu erhalten, und bietet Flexibilit\xE4t, das Ausgabeformat nach\
  \ Bedarf\u2026"
lastmod: '2024-03-13T22:44:54.319760-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell verwendet externe Befehle wie `date`, um das aktuelle Datum zu\
  \ erhalten, und bietet Flexibilit\xE4t, das Ausgabeformat nach Bedarf zu formatieren."
title: Den aktuellen Datum abrufen
weight: 29
---

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
