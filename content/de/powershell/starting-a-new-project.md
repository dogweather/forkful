---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt zu starten bedeutet, den ersten Schritt in die Entwicklung einer neuen Softwareanwendung zu machen. Programmierer machen das, um eine spezifische Problemstellung zu lösen oder um eine neue Idee zu realisieren.

## Wie mache ich das:

Hier sind einige grundlegende PowerShell-Befehle, um ein neues Projekt zu starten. 

```PowerShell
# Navigieren Sie zu Ihrem Projektverzeichnis
cd 'C:\Ihr_Projektverzeichnis'

# Erstellen Sie einen neuen Ordner für Ihr Projekt
mkdir 'Neues_Projekt'

# Navigieren Sie in den neuen Projektordner
cd 'Neues_Projekt'

# Erstellen Sie eine neue Datei
New-Item 'MeineDatei.ps1'
```

Der obige Code erstellt einen Ordner namens 'Neues_Projekt' und darin eine Datei namens 'MeineDatei.ps1'.

## Tiefere Einblicke:

Historisch gesehen hat PowerShell seinen Ursprung im Jahr 2006, als eine Alternative zu CMD und anderen Shell-Skriptsprachen. 

Als Alternative kann man Git verwenden, um ein neues Projekt zu erstellen, wenn man mit einem verteilten Team arbeitet und Versionskontrolle benötigt.

Bei der Implementierung eines neuen Projekts unterscheiden sich die Details je nach Anwendung und den spezifischen Anforderungen des Projekts. In der Regel werden jedoch Ordner und Dateien zur Strukturierung und Organisation des Codes erstellt.

## Siehe auch:

Microsoft PowerShell-Dokumentation: https://docs.microsoft.com/de-de/powershell/

So verwenden Sie Git für Ihr Projekt: https://www.git-scm.com/book/de/v2/Erste-Schritte-Git-Basiskonzepte