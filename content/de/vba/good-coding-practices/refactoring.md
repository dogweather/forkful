---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:56.109271-07:00
description: "Refactoring in der Programmierung beinhaltet das Modifizieren der Struktur\
  \ des Codes, ohne dessen Verhalten zu \xE4ndern, um Aspekte wie Lesbarkeit,\u2026"
lastmod: '2024-03-13T22:44:53.727450-06:00'
model: gpt-4-0125-preview
summary: "Refactoring in der Programmierung beinhaltet das Modifizieren der Struktur\
  \ des Codes, ohne dessen Verhalten zu \xE4ndern, um Aspekte wie Lesbarkeit, Wartbarkeit\
  \ oder Leistung zu verbessern."
title: Refactoring
weight: 19
---

## Was & Warum?

Refactoring in der Programmierung beinhaltet das Modifizieren der Struktur des Codes, ohne dessen Verhalten zu ändern, um Aspekte wie Lesbarkeit, Wartbarkeit oder Leistung zu verbessern. Programmierer nehmen ein Refactoring vor, um den Code effizienter zu gestalten, leichter verständlich und in Zukunft einfacher modifizierbar zu machen sowie die Wahrscheinlichkeit von Fehlern zu reduzieren.

## Wie:

Betrachten wir ein grundlegendes Beispiel in Visual Basic für Applikationen (VBA), bei dem wir eine Subroutine haben, die Details eines Mitarbeiters ausgibt. Anfänglich ist der Code unübersichtlich, schwer zu pflegen oder zu erweitern.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim alter As Integer
    Dim abteilung As String
    name = "John Doe"
    alter = 30
    abteilung = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Alter: " & alter & vbCrLf & "Abteilung: " & abteilung
End Sub
```

Refactoring-Schritt 1: Methode extrahieren. Eine der häufigsten Refactoring-Techniken besteht darin, einen bestimmten Codeabschnitt zu nehmen und ihn in seine eigene Methode zu verschieben. Dies macht den Code modularer und leichter verständlich.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim alter As Integer
    Dim abteilung As String
    name = "John Doe"
    alter = 30
    abteilung = "IT"
    
    DisplayMessage name, alter, abteilung
End Sub

Private Sub DisplayMessage(name As String, alter As Integer, abteilung As String)
    MsgBox "Name: " & name & vbCrLf & "Alter: " & alter & vbCrLf & "Abteilung: " & abteilung
End Sub
```

Refactoring-Schritt 2: Nutzung einer Struktur. Dieser Schritt beinhaltet die Verwendung einer Datenstruktur, um verwandte Daten zu speichern, was die Klarheit des Codes verbessert und das Weitergeben von gruppierter Information erleichtert.

```vb
Type Mitarbeiter
    name As String
    alter As Integer
    abteilung As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Mitarbeiter
    emp.name = "John Doe"
    emp.alter = 30
    emp.abteilung = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Mitarbeiter)
    MsgBox "Name: " & emp.name & vbCrLf & "Alter: " & emp.alter & vbCrLf & "Abteilung: " & emp.abteilung
End Sub
```

Diese Schritte verwandeln unübersichtlichen Code in modularen, strukturierten Code und verbessern deutlich die Lesbarkeit und Wartbarkeit.

## Vertiefung

Das Konzept des Refactorings ist so alt wie die Programmierung selbst, aber es war Martin Fowlers Buch „Refactoring: Improving the Design of Existing Code“, das es in den Mainstream brachte und seine Bedeutung im Softwareentwicklungsprozess hervorhob. In Visual Basic für Applikationen kann Refactoring aufgrund des Mangels an eingebauten Tools, die in moderneren integrierten Entwicklungsumgebungen (IDEs) eine automatisierte Refaktorisierung unterstützen, etwas herausfordernder sein.

Das mindert jedoch nicht dessen Wichtigkeit. Auch in VBA kann die Anwendung grundlegender Refactoring-Techniken von Hand die Codebasis erheblich verbessern und sie sauberer und effizienter machen. Während VBA vielleicht nicht die gleichen modernen Annehmlichkeiten bietet, bleiben die Prinzipien guten Code-Designs universell. Entwickler, die aus anderen Sprachen kommen, mögen den manuellen Prozess vielleicht mühsam finden, werden aber zweifellos die Vorteile der Investition in die Verbesserung der Codequalität von Anfang an zu schätzen wissen.

Für robustere Entwicklungsumgebungen oder bei der Arbeit an besonders anspruchsvollen Projekten könnte es sich lohnen, Alternativen zu erkunden, die leistungsfähigere Refactoring-Tools bieten, oder VBA-Projekte in eine .NET-Sprache zu konvertieren, wo Visual Studio umfangreiche Refactoring-Unterstützung bietet. Dennoch ist das Verständnis und die Anwendung von Refactoring-Prinzipien in VBA eine wertvolle Fähigkeit, die die Bedeutung von sauberem, wartbarem Code unterstreicht, egal in welcher Umgebung.
