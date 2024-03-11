---
date: 2024-01-26 04:16:39.613773-07:00
description: "Die interaktive Shell oder die Lese-Auswertungs-Druck-Schleife (REPL)\
  \ erm\xF6glicht es Ihnen, PowerShell-Befehle einzugeben und sofortiges Feedback\
  \ zu\u2026"
lastmod: '2024-03-11T00:14:28.005192-06:00'
model: gpt-4-0125-preview
summary: "Die interaktive Shell oder die Lese-Auswertungs-Druck-Schleife (REPL) erm\xF6\
  glicht es Ihnen, PowerShell-Befehle einzugeben und sofortiges Feedback zu\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Die interaktive Shell oder die Lese-Auswertungs-Druck-Schleife (REPL) ermöglicht es Ihnen, PowerShell-Befehle einzugeben und sofortiges Feedback zu erhalten. Programmierer nutzen sie, um Code-Schnipsel schnell zu testen, zu debuggen oder neue Befehle zu lernen, ohne ein vollständiges Skript zu schreiben.

## Wie:
Starten Sie PowerShell und Sie befinden sich in der REPL. Probieren Sie das Cmdlet `Get-Date` aus:

```PowerShell
PS > Get-Date
```

Sie sollten die aktuelle Datum- und Zeitangabe sehen:

```PowerShell
Mittwoch, 31. März 2023 12:34:56
```

Jetzt verketten Sie Befehle. Sortieren wir Prozesse nach Speicherverbrauch:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Dies gibt die obersten 5 Prozesse nach Größe des Arbeitssets (Speicherverbrauch) aus.

## Tiefer Eintauchen
Die REPL von PowerShell hat ihre Wurzeln in der Unix-Shell und anderen dynamischen Sprach-Shells wie Pythons. Sie ist eine Einzelbenutzer-, interaktive Befehlsausführungsumgebung. Im Gegensatz zu einer kompilierten Sprache, bei der Sie ganze Anwendungen schreiben und dann kompilieren, ermöglicht eine REPL-Umgebung das Schreiben und Ausführen von Code Zeile für Zeile. PowerShell unterstützt auch die Ausführung von Skripten für größere Aufgaben.

Alternativen für Windows beinhalten die Eingabeaufforderung oder andere sprachspezifische REPLs wie IPython. In der Unix/Linux-Welt dienen Shells wie bash oder zsh einer ähnlichen Funktion.

Die Implementierung von PowerShell verwendet eine Host-Anwendung, um die Shell auszuführen. Obwohl PowerShell.exe in Windows am häufigsten ist, können auch andere wie die Integrated Scripting Environment (ISE) oder das integrierte Terminal von Visual Studio Code als Host dienen.

## Siehe Auch
- [Über PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
