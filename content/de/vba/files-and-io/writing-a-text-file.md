---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:45.987220-07:00
description: "Das Schreiben einer Textdatei in Visual Basic for Applications (VBA)\
  \ umfasst das Erstellen, Modifizieren oder Anh\xE4ngen von Textdaten an Dateien,\
  \ eine\u2026"
lastmod: 2024-02-19 22:05:12.663068
model: gpt-4-0125-preview
summary: "Das Schreiben einer Textdatei in Visual Basic for Applications (VBA) umfasst\
  \ das Erstellen, Modifizieren oder Anh\xE4ngen von Textdaten an Dateien, eine\u2026"
title: Eine Textdatei schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei in Visual Basic for Applications (VBA) umfasst das Erstellen, Modifizieren oder Anhängen von Textdaten an Dateien, eine grundlegende Aufgabe für die Speicherung von Ausgaben, Logging oder die Interaktion mit anderen Anwendungen. Programmierer nutzen diese Funktionalität, um Berichterstellung, Datenexport oder die Generierung von Konfigurationsdateien im Microsoft Office-Ökosystem zu automatisieren.

## Wie geht das:

VBA bietet mehrere Methoden, um in eine Datei zu schreiben, aber einer der einfachsten Wege ist die Verwendung des `FileSystemObject`. Hier ist eine Schritt-für-Schritt-Anleitung zum Erstellen einer einfachen Textdatei und zum Schreiben von Daten in sie:

1. **Microsoft Scripting Runtime referenzieren**: Zuerst stellen Sie sicher, dass Ihr VBA-Editor Zugriff auf das `FileSystemObject` hat. Gehen Sie in dem VBA-Editor zu Extras > Verweise und setzen Sie ein Häkchen bei "Microsoft Scripting Runtime".

2. **Eine Textdatei erstellen**: Das folgende VBA-Code-Snippet zeigt, wie man eine Textdatei erstellt und eine Zeile Text hineinschreibt.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' CreateTextFile Parameter: (Dateiname, Überschreiben, Unicode)
    Set textFile = fso.CreateTextFile("C:\yourPath\beispiel.txt", True, False)
    
    ' Eine Zeile Text schreiben
    textFile.WriteLine "Hallo, VBA!"
    
    ' Die Datei schließen
    textFile.Close
End Sub
```

Dieses Skript erstellt (oder überschreibt, falls bereits vorhanden) eine Datei mit dem Namen `beispiel.txt` im angegebenen Verzeichnis und schreibt "Hallo, VBA!" hinein, bevor die Datei geschlossen wird, um die Änderungen zu speichern.

3. **Beispielausgabe**:

Nach dem Ausführen des oben genannten VBA-Skripts finden Sie eine Datei mit dem Namen `beispiel.txt` und folgendem Inhalt:

```
Hallo, VBA!
```

## Vertiefung:

Das `FileSystemObject` (FSO), Teil der Microsoft Scripting Runtime-Bibliothek, bietet einen umfangreichen Satz an Eigenschaften und Methoden für Dateioperationen, der über das hinausgeht, was traditionelle VBA-Dateibehandlung bietet (z. B. `Open`, `Print` #, `Write` #). Neben der Dateibehandlung kann FSO auch Ordner und Laufwerke manipulieren, was es zu einem leistungsstarken Werkzeug für Dateisystemoperationen innerhalb von VBA macht.

Es ist jedoch zu beachten, dass FSO, obwohl es einen moderneren Ansatz für Dateioperationen in VBA darstellt, für einfache Aufgaben im Vergleich zu den nativen Dateibehandlungsbefehlen von VBA Overhead verursachen kann. Darüber hinaus könnte die Portabilität und Kompatibilität mit anderen Systemen (z. B. frühere Office-Versionen, Mac Office) wegen der Zugehörigkeit zu einer externen Bibliothek Anlass zur Sorge geben.

In Kontexten, in denen Leistung, Kompatibilität oder minimale externe Abhängigkeiten kritisch sind, könnten Programmierer die eingebauten Dateibehandlungstechniken von VBA in Betracht ziehen. Jedoch, für kompliziertere Operationen oder beim Arbeiten in einer Umgebung, in der diese Bedenken gemildert sind (wie in einem kontrollierten Unternehmensumfeld), überwiegen die Vorteile des FileSystemObject oft seine Nachteile.
