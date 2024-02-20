---
date: 2024-01-20 17:32:45.268915-07:00
description: "Das Vergleichen von zwei Daten bedeutet, sie hinsichtlich ihrer zeitlichen\
  \ Reihenfolge zu pr\xFCfen. Programmierer machen das, um Zeitabschnitte zu\u2026"
lastmod: 2024-02-19 22:05:13.260889
model: gpt-4-1106-preview
summary: "Das Vergleichen von zwei Daten bedeutet, sie hinsichtlich ihrer zeitlichen\
  \ Reihenfolge zu pr\xFCfen. Programmierer machen das, um Zeitabschnitte zu\u2026"
title: Vergleich von zwei Daten
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten bedeutet, sie hinsichtlich ihrer zeitlichen Reihenfolge zu prüfen. Programmierer machen das, um Zeitabschnitte zu berechnen, Fristen zu überwachen oder Chronologien zu ermitteln.

## So geht's:
In Fish nutzt man oft `date`, um mit Daten zu hantieren. Hier ein schneller Weg, um zwei Daten zu vergleichen:

```Fish Shell
set date1 (date -ud '2023-03-15' +%s)
set date2 (date -ud '2023-03-20' +%s)

if test $date1 -lt $date2
    echo "Datum1 ist früher als Datum2."
else if test $date1 -eq $date2
    echo "Datum1 und Datum2 sind gleich."
else
    echo "Datum1 ist später als Datum2."
end
```

Ausgabe, je nach Vergleich:
```
Datum1 ist früher als Datum2.
```
oder
```
Datum1 und Datum2 sind gleich.
```
oder
```
Datum1 ist später als Datum2.
```

## Tiefgang:
Das Konzept des Datenvergleichs gibt es, seit Computer Zeitkonzepte verwalten. In Unix-artigen Systemen wie denen, in denen Fish läuft, wird oft die Anzahl der Sekunden seit dem 1. Januar 1970 (der sogenannten "Unix-Epoche") verglichen.

Es gibt Alternativen zu `date` wie z.B. `strtotime` in PHP oder `datetime` Module in Python, die ähnliche Funktionalitäten bieten. Aber in einer Shell, besonders in Fish, sind eingebaute Funktionen oft schneller und leichter zu schreiben.

Die Implementierungsdetails variieren. Für Fish und meisten Unix-Systeme konvertiert `date +%s` ein Datumsstring in eine Ganzzahl, die die Sekunden seit der Epoche darstellt. Das macht den Vergleich einfach und effizient: die größere Zahl ist die spätere Zeit.

## Siehe auch:
- Der Fish Shell Cookbook für praxisnahe Beispiele: [Fish Shell Cookbook](https://github.com/jorgebucaran/cookbook.fish)
- Das Thema Zeitmessung in Programmiersprachen allgemein: [Epoch Converter](https://www.epochconverter.com/programming/)
