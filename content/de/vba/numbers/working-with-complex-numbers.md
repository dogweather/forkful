---
title:                "Arbeiten mit komplexen Zahlen"
aliases:
- /de/vba/working-with-complex-numbers/
date:                  2024-02-01T22:08:05.738369-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit komplexen Zahlen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit komplexen Zahlen umfasst das Ausführen mathematischer Operationen mit Zahlen, die sowohl einen Realteil als auch einen Imaginärteil haben. Programmierer beschäftigen sich oft mit komplexen Zahlen in Bereichen wie Ingenieurwesen, Physik und überall dort, wo es darum geht, Gleichungen zu lösen, die mit reinen reellen Zahlen nicht möglich sind.

## Wie geht das:

In Visual Basic for Applications (VBA) ist der Umgang mit komplexen Zahlen im Vergleich zu Sprachen mit nativer Unterstützung für diese etwas weniger direkt. Jedoch können Sie komplexe Operationen durch Erstellen von Funktionen oder Verwenden vorhandener Bibliotheksfunktionen bewältigen. Lassen Sie uns ein grundlegendes Beispiel für Addition, Subtraktion, Multiplikation und Division von komplexen Zahlen erkunden:

```vb
' Funktion, um komplexe Zahlen zu addieren
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Extrahieren der Real- und Imaginärteile aus den komplexen Zahlen
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Durchführen der Addition
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Beispielverwendung
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Ergebnis der Addition: " & result  ' Ausgabe: Ergebnis der Addition: 4+9i
End Sub
```

Während dies die Addition demonstriert, können ähnliche Ansätze für Subtraktion, Multiplikation und Division angepasst werden. Für komplexe Operationen über die Grundrechenarten hinaus könnte es sich lohnen, externe Bibliotheken zu erkunden oder andere Lösungen zu integrieren, die Operationen mit komplexen Zahlen nativer unterstützen.

## Tiefergehende Betrachtung:

VBA enthält keine integrierte Unterstützung für komplexe Zahlen, ein Bereich, in dem es hinter Sprachen wie Python zurückbleibt, das über eine komplexe Zahlenklasse (`complex`) verfügt, oder C++ mit seiner Standard Template Library (`std::complex`). Historisch gesehen ist die Notwendigkeit, direkt in VBA komplexe Zahlen zu manipulieren, relativ selten, da es oft für Automatisierung, die Manipulation von Office-Anwendungen und Aufgaben verwendet wird, die traditionell keine komplexen mathematischen Berechnungen erfordern. Als VBA konzipiert und entwickelt wurde, konzentrierten sich die Anwendungsfälle hauptsächlich auf geschäftliche Anwendungen anstatt auf wissenschaftliches Rechnen, was das Fehlen erklären könnte.

Für Aufgaben, die umfangreiche Manipulationen komplexer Zahlen erfordern, könnten Programmierer die Verwendung einer mathematisch orientierteren Sprache als vorteilhaft empfinden. Jedoch, für diejenigen, die sich auf die Verwendung von VBA festgelegt haben oder darauf beschränkt sind, sind das Schreiben benutzerdefinierter Funktionen (wie dargestellt) oder die Integration mit Software, die diese Fähigkeiten besitzt (wie z.B. MATLAB oder bis zu einem gewissen Grad Excel selbst), gangbare Wege. Trotz seiner Einschränkungen können kreative Lösungen und externe Integrationen die Nützlichkeit von VBA in Bereiche erweitern, für die es ursprünglich nicht konzipiert war, einschließlich der Arbeit mit komplexen Zahlen.
