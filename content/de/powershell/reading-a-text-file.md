---
title:                "Eine Textdatei lesen"
html_title:           "PowerShell: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei ist eine häufige Aufgabe für Programmierer. Dabei geht es darum, den Inhalt einer Datei im Klartext auszulesen und damit weiterzuarbeiten. Programmierer nutzen diese Funktion z.B. um Konfigurationsdateien auszulesen oder um Textdaten aus verschiedenen Quellen zu vereinen.

## Wie geht's?
Um eine Textdatei in PowerShell auszulesen, gibt es verschiedene Möglichkeiten. Eine einfache und effektive Methode ist die Verwendung des Befehls `Get-Content`. Hier ein Beispiel:

```PowerShell
$inhalt = Get-Content C:\Beispiel\textdatei.txt
```

Dieser Befehl liest den gesamten Inhalt der Textdatei in die Variable `$inhalt` ein. Man kann auch angeben, wie viel Zeilen man auslesen möchte, indem man eine Zahl als zweites Argument beim Aufruf angibt.

```PowerShell
$zeilen = Get-Content C:\Beispiel\textdatei.txt -TotalCount 10
```

Dieser Befehl liest die ersten 10 Zeilen der Textdatei in die Variable `$zeilen` ein.

## Tiefere Einblicke
Das Lesen von Textdateien hat eine lange Geschichte in der Programmierung. In früheren Programmiersprachen musste man den Inhalt einer Datei Zeile für Zeile selbst einlesen und verarbeiten. In PowerShell gibt es jedoch den Befehl `Get-Content`, der diese Aufgabe wesentlich einfacher und effizienter macht.

Alternativ zum Befehl `Get-Content` können auch Befehle wie `Select-String` oder `Import-Csv` verwendet werden, um spezifische Inhalte aus einer Textdatei zu lesen. Diese Befehle bieten weitere Funktionen und Optionen, die je nach Bedarf eingesetzt werden können.

## Siehe auch
- [Dokumentation zu "Get-Content"](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7)
- [Artikel zu Textdateien in PowerShell](https://www.thomasmaurer.ch/2019/06/working-with-text-files-in-powershell/)