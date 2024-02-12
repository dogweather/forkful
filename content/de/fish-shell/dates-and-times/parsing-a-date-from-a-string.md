---
title:                "Einen Datum aus einem String analysieren"
aliases:
- /de/fish-shell/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:09.719117-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet das Extrahieren von Datumsinformationen, die innerhalb von Strings kodiert sind, und die Umwandlung in ein strukturiertes Format, das von Programmierumgebungen erkannt und manipuliert werden kann. Programmierer tun dies, um Operationen wie Datumsvergleich, Arithmetik, Formatierung und Lokalisierung zu ermöglichen, die für die effiziente Handhabung von Zeitplänen, Zeitstempeln und historischen Daten in Software unerlässlich sind.

## Wie:
In Fish Shell gibt es keine eingebauten Befehle, die speziell für das Parsen von Daten aus Strings entwickelt wurden. Stattdessen verlässt man sich auf externe Dienstprogramme wie `date` (verfügbar unter Linux und macOS) oder nutzt beliebte Drittanbieter-Tools wie `GNU date` für komplexeres Parsen. So geht man vor:

**Verwendung von `date` mit Fish:**

Um einen Datumsstring im Format "JJJJ-MM-TT" zu parsen, können Sie den `date`-Befehl mit der Option `-d` (oder `--date` für GNU date) gefolgt von dem String verwenden. Die Option `+` wird verwendet, um das Ausgabeformat zu bestimmen.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Ausgabe: Samstag, 01 April 2023
```

Für macOS (das ein anderes Format für die Flags `-j` und `-f` benötigt):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Ausgabe: Samstag, 01 April 2023
```

**Verwendung von GNU `date` für komplexes Parsen:**

GNU `date` ist flexibler in Bezug auf Stringformate. Es kann viele gängige Datumsstringformate automatisch erkennen, ohne dass das Eingabeformat explizit angegeben werden muss:

```fish
set complex_date_str "1. April 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Ausgabe: 2023-04-01 14:00:00
```

Wenn jedoch mit Datumsstrings gearbeitet wird, die möglicherweise nicht automatisch erkannt werden oder wenn eine präzise Kontrolle über das Eingabeformat benötigt wird, wird die Angabe des Eingabeformats mit GNU `date` nicht direkt unterstützt. In solchen Fällen sollten Sie die Vorverarbeitung des Strings in Betracht ziehen oder ein anderes Tool verwenden, das für komplexere Datums-Parsing-Routinen konzipiert ist.
