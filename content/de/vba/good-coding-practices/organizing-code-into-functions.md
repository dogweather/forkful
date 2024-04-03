---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:29.608672-07:00
description: "Code in Funktionen in Visual Basic f\xFCr Anwendungen (VBA) zu organisieren,\
  \ bedeutet, ein Programm in kleinere, handhabbare St\xFCcke, bekannt als Funktionen,\u2026"
lastmod: '2024-03-13T22:44:53.724259-06:00'
model: gpt-4-0125-preview
summary: "Code in Funktionen in Visual Basic f\xFCr Anwendungen (VBA) zu organisieren,\
  \ bedeutet, ein Programm in kleinere, handhabbare St\xFCcke, bekannt als Funktionen,\
  \ zu unterteilen."
title: Organisation von Code in Funktionen
weight: 18
---

## Wie man das macht:
In VBA werden Funktionen mit den Anweisungen `Function` und `End Function` definiert. Hier ist ein einfaches Beispiel, wie man eine Funktion erstellt, die den Flächeninhalt eines Rechtecks berechnet:

```basic
Function CalculateArea(Laenge As Double, Breite As Double) As Double
    CalculateArea = Laenge * Breite
End Function
```

Um diese Funktion in Ihrem VBA-Code aufzurufen und das Ergebnis in einem Nachrichtenfeld anzuzeigen, würden Sie verwenden:

```basic
Sub ShowArea()
    Dim flaeche As Double
    flaeche = CalculateArea(10, 5)
    MsgBox "Die Fläche beträgt " & flaeche
End Sub
```

Wenn dieser Code ausgeführt wird, zeigt ein Nachrichtenfeld an: `Die Fläche beträgt 50`.

### Variablen ByRef und ByVal übergeben
VBA ermöglicht es Ihnen, Variablen an Funktionen entweder per Referenz (`ByRef`) oder per Wert (`ByVal`) zu übergeben. Ersteres bedeutet, dass die ursprüngliche Variable von der Funktion geändert werden kann, während letzteres eine Kopie übergibt und die ursprüngliche Variable vor Änderungen schützt.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Tiefergehend
VBA, als eine ereignisgesteuerte Programmiersprache, legt bedeutenden Wert auf Funktionen und Subroutinen, um verschiedene Aufgaben zu bewältigen. Im Gegensatz zu vielen modernen Sprachen hat VBA eine einzigartige Eigenschaft, bei der das Schlüsselwort `Function` nicht nur einen Block wiederverwendbaren Codes deklariert, sondern auch einen impliziten Rückgabewert ermöglicht, der direkt dem Namen der Funktion zugewiesen wird.

Historisch gesehen wurde das Design von VBA-Funktionen von früheren Programmierparadigmen beeinflusst, bei denen die Kapselung und Modularität allmählich für ihre Bedeutung in der Softwareentwicklung anerkannt wurden. Dieser historische Hintergrund hat dazu geführt, dass VBA einen etwas konservativen, aber funktionalen Ansatz zur Organisation von Code angenommen hat.

Während VBA innerhalb seiner nativen Umgebungen (z. B. Microsoft Office-Anwendungen) leistungsfähig ist, ist es wichtig zu beachten, dass sich die Programmierwelt weiterentwickelt hat. Sprachen wie Python bieten eine einfachere Syntax und eine umfangreiche Standardbibliothek, was sie zu einer günstigen Alternative für verschiedene Anwendungen außerhalb der Office-Suite macht. Jedoch, wenn man innerhalb von Microsoft Office-Produkten arbeitet, sind die Integrations- und Automatisierungsfähigkeiten, die VBA bietet, unübertroffen.

Es lohnt sich zu erwähnen, dass trotz seines Alters die Gemeinschaft um VBA aktiv bleibt und kontinuierlich innovative Wege findet, seine Funktionalität zu nutzen. Dennoch, da die Softwareindustrie sich in Richtung modernerer, vielseitigerer und robusterer Sprachen bewegt, werden Programmierer, die mit VBA vertraut sind, ermutigt, diese Alternativen für nicht Office-bezogene Aufgaben zu erkunden, um ihr Programmierwerkzeug zu erweitern.
