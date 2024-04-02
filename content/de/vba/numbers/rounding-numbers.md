---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:19.199866-07:00
description: "Das Runden von Zahlen in der Programmierung geht darum, eine Zahl auf\
  \ ihre n\xE4chstgelegene ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen\
  \ zu\u2026"
lastmod: '2024-03-13T22:44:53.712003-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen in der Programmierung geht darum, eine Zahl auf ihre\
  \ n\xE4chstgelegene ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen\
  \ zu\u2026"
title: Runden von Zahlen
weight: 13
---

## Was & Warum?

Das Runden von Zahlen in der Programmierung geht darum, eine Zahl auf ihre nächstgelegene ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen zu approximieren. Programmierer runden Zahlen, um Zahlen zu vereinfachen, die Lesbarkeit zu verbessern oder spezifischen numerischen Kriterien in Berechnungen zu entsprechen, besonders bei finanziellen Berechnungen, wo Präzision von Bedeutung ist.

## Wie:

In Visual Basic for Applications (VBA) kann das Runden durch mehrere Funktionen erreicht werden, von denen jede für spezifische Szenarien geeignet ist. Hier sind die am häufigsten verwendeten Funktionen mit Beispielen:

1. **Round-Funktion**:
   Die `Round`-Funktion rundet eine Zahl auf eine bestimmte Anzahl von Stellen.
   ```basic
   Dim gerundeteZahl As Double
   gerundeteZahl = Round(3.14159, 2)  ' Ausgabe: 3.14
   MsgBox gerundeteZahl
   ```
   
2. **Int- und Fix-Funktionen**:
   Sowohl die `Int`- als auch die `Fix`-Funktionen werden verwendet, um Zahlen auf die nächste ganze Zahl abzurunden, sie verhalten sich jedoch bei negativen Zahlen unterschiedlich.
   ```basic
   Dim intGerundet As Integer
   Dim fixGerundet As Integer
   
   intGerundet = Int(-3.14159)  ' Ausgabe: -4
   fixGerundet = Fix(-3.14159)  ' Ausgabe: -3
   
   MsgBox "Int: " & intGerundet & ", Fix: " & fixGerundet
   ```

3. **Ceiling- und Floor-Funktionen**:
   VBA verfügt nicht über eingebaute `Ceiling`- und `Floor`-Funktionen, wie sie in anderen Sprachen zu finden sind. Um dies zu simulieren, verwenden Sie `Application.WorksheetFunction.Ceiling_Math` und `Application.WorksheetFunction.Floor_Math` für Excel VBA.
   ```basic
   Dim ceilingZahl As Double
   Dim floorZahl As Double
   
   ceilingZahl = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Ausgabe: 4
   floorZahl = Application.WorksheetFunction.Floor_Math(3.14159)  ' Ausgabe: 3
   
   MsgBox "Ceiling: " & ceilingZahl & ", Floor: " & floorZahl
   ```

## Vertiefung

Die `Round`-Funktion in VBA unterscheidet sich grundsätzlich von Rundungsmethoden in anderen Sprachen durch die Verwendung des **Banker’s Rounding**. Banker’s Rounding rundet auf die nächstgelegene gerade Zahl, wenn genau in der Mitte zwischen zwei Zahlen, was eine Verringerung der Verzerrung in Berechnungen über einen großen Datensatz ermöglicht und ein statistisch signifikanteres Ergebnis liefert. Dies kann jedoch zu unerwartetem Verhalten führen, insbesondere wenn in jedem Fall eine genaue Ganzzahligkeit erwartet wird, für diejenigen, die damit nicht vertraut sind.

Im Gegensatz dazu verwenden viele Programmiersprachen und Systeme das "arithmetische Runden" oder "Half-up-Rounding", bei dem eine Zahl genau zwischen zwei möglichen gerundeten Werten immer aufgerundet wird. Beim Übersetzen oder Portieren von Code aus anderen Sprachen nach VBA müssen Programmierer diese Unterschiede im Auge behalten, um subtile Fehler oder Ungenauigkeiten in finanziellen und statistischen Anwendungen zu vermeiden.

Obwohl VBA eine Vielzahl von Funktionen für das Runden bietet, hebt das Fehlen der `Ceiling`- und `Floor`-Funktionen (ohne auf Excels WorksheetFunction zurückzugreifen) eine Einschränkung in seinen nativen Fähigkeiten hervor. Programmierer, die aus umfangreicheren Sprachen kommen, könnten diese Auslassungen als unpraktisch empfinden und müssen möglicherweise benutzerdefinierte Lösungen implementieren oder ihre Berechnungen anpassen, um die verfügbaren Funktionen zu nutzen. Trotz dieser Einschränkungen kann das Verständnis und die korrekte Verwendung der Rundungsfunktionen von VBA helfen, sicherzustellen, dass numerische Berechnungen sowohl genau sind als auch den Anforderungen der meisten Anwendungen entsprechen.
