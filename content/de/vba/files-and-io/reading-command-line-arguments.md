---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:18.921292-07:00
description: "Das Lesen von Befehlszeilenargumenten in Visual Basic for Applications\
  \ (VBA) beinhaltet den Zugriff auf Parameter, die beim Ausf\xFChren Ihres Programms\u2026"
lastmod: '2024-03-13T22:44:53.735427-06:00'
model: gpt-4-0125-preview
summary: "Das Lesen von Befehlszeilenargumenten in Visual Basic for Applications (VBA)\
  \ beinhaltet den Zugriff auf Parameter, die beim Ausf\xFChren Ihres Programms\u2026"
title: Lesen von Befehlszeilenargumenten
weight: 23
---

## Was & Warum?

Das Lesen von Befehlszeilenargumenten in Visual Basic for Applications (VBA) beinhaltet den Zugriff auf Parameter, die beim Ausführen Ihres Programms übergeben werden. Diese Technik wird häufig verwendet, um das Verhalten oder die Ausgabe eines Programms ohne die Notwendigkeit einer Benutzerinteraktion zu beeinflussen, was Automatisierungs- und Skriptaufgaben erheblich einfacher und vielseitiger macht.

## Wie:

Im Gegensatz zu einfacheren Programmierumgebungen verfügt VBA nicht über eine integrierte Funktion, um Befehlszeilenargumente im herkömmlichen Sinne direkt zu lesen, da es hauptsächlich für die Einbettung in Microsoft Office-Anwendungen konzipiert ist. Mit ein wenig Kreativität können wir jedoch Windows Script Host (WSH) verwenden oder externe APIs aufrufen, um eine ähnliche Funktionalität zu erreichen. Hier ist ein praktischer Workaround mit WSH:

1. **Erstellen Sie ein VBScript, um Argumente an VBA zu übergeben:**

   Schreiben Sie zuerst eine VBScript-Datei (*yourScript.vbs*), die Ihre VBA-Anwendung (z. B. ein Excel-Makro) startet und die Befehlszeilenargumente übergibt:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Zugriff auf die Argumente in VBA:**

   In Ihrer VBA-Anwendung (*YourMacroWorkbook.xlsm*), ändern oder erstellen Sie das Makro (*YourMacroName*), um Parameter zu akzeptieren:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **Führen Sie Ihr Skript aus:**

   Führen Sie das VBScript von der Befehlszeile aus, wobei Sie bei Bedarf Argumente übergeben:

```shell
cscript yourScript.vbs "Hello" "World"
```

   Dies sollte dazu führen, dass Ihr VBA-Makro mit den Argumenten "Hello" und "World" ausgeführt wird und diese in einem Nachrichtenfeld angezeigt werden.

## Vertiefung:

Im historischen Kontext wurde VBA entwickelt, um die Fähigkeiten von Microsoft Office-Anwendungen zu erweitern, und nicht als eigenständige Programmierumgebung. Daher liegt die direkte Interaktion mit der Befehlszeile außerhalb seines Hauptanwendungsbereichs, was das Fehlen einer integrierten Unterstützung für das Lesen von Befehlszeilenargumenten erklärt.

Die oben dargelegte Methode, obwohl wirkungsvoll, ist mehr eine Umgehungslösung als eine native Lösung, die externe Skripterstellung nutzt, um die Lücke zu überbrücken. Dieser Ansatz kann Komplexität und potenzielle Sicherheitsbedenken einführen, da er das Aktivieren von Makros und möglicherweise das Senken von Sicherheitseinstellungen zur Ausführung erfordert.

Für Aufgaben, die stark auf Befehlszeilenargumente angewiesen sind oder eine nahtlosere Integration mit dem Windows-Betriebssystem benötigen, könnten andere Programmiersprachen wie PowerShell oder Python robustere und sicherere Lösungen bieten. Diese Alternativen bieten direkte Unterstützung für Befehlszeilenargumente und eignen sich besser für eigenständige Anwendungen oder Skripte, die externe Eingaben benötigen, um ihr Verhalten dynamisch zu ändern.
