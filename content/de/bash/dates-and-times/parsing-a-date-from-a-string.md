---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:39.708621-07:00
description: "Das Parsen eines Datums aus einem String in Bash beinhaltet das Extrahieren\
  \ und Konvertieren von Datumsinformationen aus Textdaten in ein Format, das Bash\u2026"
lastmod: 2024-02-19 22:05:12.999748
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String in Bash beinhaltet das Extrahieren\
  \ und Konvertieren von Datumsinformationen aus Textdaten in ein Format, das Bash\u2026"
title: Einen Datum aus einem String analysieren
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String in Bash beinhaltet das Extrahieren und Konvertieren von Datumsinformationen aus Textdaten in ein Format, das Bash manipulieren oder für weitere Prozesse verwenden kann. Dies ist eine gängige Anforderung beim Scripting für Aufgaben wie die Analyse von Logdateien, die Organisation von Dateien basierend auf Datumsstempeln oder automatisierte Berichte, was es zu einer wichtigen Fähigkeit für Programmierer macht, zeitliche Daten effektiv zu verwalten und zu nutzen.

## Wie geht das:

Bash selbst ist in direkten Datums-Parsing-Fähigkeiten ziemlich begrenzt und verlässt sich oft auf externe Tools wie `date` und `awk` für komplexere Manipulationen. Hier ist, wie Sie ein spezifisches Format parsen und es dann mit dem `date` Befehl konvertieren oder Operationen ausführen können.

**Beispiel 1:** Extrahieren eines Datumsstrings und Konvertieren in ein anderes Format.

Nehmen wir an, Sie haben ein Datum im Format `jjjj-mm-tt` und möchten es in `tt-mm-jjjj` konvertieren.

```bash
original_datum="2023-04-01"
formatiertes_datum=$(date -d $original_datum '+%d-%m-%Y')

echo $formatiertes_datum
```

**Beispielausgabe:**
```
01-04-2023
```

Dies verwendet den `date` Befehl mit der Option `-d`, um den Eingabedatumsstring anzugeben, und `+%d-%m-%Y`, um das Ausgabeformat zu formatieren.

**Beispiel 2:** Verwendung von `awk` zum Parsen eines Datums aus einer strukturierten Textzeile und dessen Konvertierung.

Angenommen, Sie haben eine Logfile-Zeile:

```
2023-04-01 12:00:00 Benutzer eingeloggt
```

Sie können den Datumsbestandteil mithilfe von `awk` und `date` extrahieren und konvertieren.

```bash
log_zeile="2023-04-01 12:00:00 Benutzer eingeloggt"
datums_teil=$(echo $log_zeile | awk '{print $1}')
formatiertes_datum=$(date -d $datums_teil "+%A, %B %d, %Y")

echo $formatiertes_datum
```

**Beispielausgabe:**
```
Samstag, April 01, 2023
```

Dieses Beispiel verwendet `awk`, um die Logzeile zu teilen und den Datumsbestandteil zu extrahieren (`$1` stellt das erste, durch Leerzeichen getrennte Feld dar), und dann wird `date` verwendet, um es neu zu formatieren.

### Verwendung von Drittanbieter-Tools

Für komplexere Parsing-Aufgaben oder wenn man es mit einer Vielzahl von Datumsformaten zu tun hat, können Drittanbieter-Tools wie `dateutils` sehr praktisch sein.

**Beispiel mit `dateutils`:**

Angenommen, Sie haben einen Datumsstring in einem nicht standardisierten Format, zum Beispiel `April 01, 2023`.

```bash
original_datum="April 01, 2023"
formatiertes_datum=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_datum)

echo $formatiertes_datum
```

**Beispielausgabe:**
```
2023-04-01
```

Dieser Befehl verwendet `dateconv` von `dateutils`, wobei das Eingabeformat mit `-i` und das gewünschte Ausgabeformat mit `-f` angegeben wird. `dateutils` unterstützt eine große Bandbreite an Datums- und Zeitformaten, was es sehr vielseitig für Datums-Parsing-Aufgaben in Bash-Skripten macht.
