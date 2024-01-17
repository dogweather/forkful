---
title:                "Debug-Ausgabe drucken"
html_title:           "PowerShell: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Drucken von Debug-Ausgaben ist ein gängiges Verfahren unter Programmierern, um Fehler und Probleme in ihrem Code zu identifizieren und zu beheben. Dabei werden zusätzliche Informationen während der Ausführung des Codes ausgegeben, um den Ablauf und die Werte von Variablen zu überprüfen.

## Wie geht's?
Um Debug-Ausgaben in PowerShell zu drucken, kannst du das Befehllet ```Write-Debug``` verwenden. Hier ist ein Beispielcode:

```
$var = "Hallo Welt"
Write-Debug "$var"
```

Die Ausgabe wird dann in der Konsole angezeigt, wenn du den Skript mit der Option ```-Debug``` ausführst:

```
PS C:\> .\meinSkript.ps1 -Debug
DEBUG: Hallo Welt
```

## Tiefer gehend
Debug-Ausgaben gab es schon lange vor PowerShell, und viele andere Programmiersprachen haben ähnliche Funktionen wie die ```Write-Debug``` Befehl. Alternativ kannst du auch den Befehl ```Write-Verbose``` verwenden, um zusätzliche Informationen während der Ausführung zu drucken.

Die Implementation von Debug-Ausgaben ist oft eine persönliche Entscheidung des Programmierers und kann je nach Anwendungsfall variieren. Es ist jedoch wichtig, sicherzustellen, dass alle Debug-Ausgaben vor dem Release einer Anwendung entfernt werden.

## Siehe auch
[Offizielle Dokumentation zu Write-Debug](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7)

[Weitere Informationen zu Debugging in PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-debugging-revisited/)