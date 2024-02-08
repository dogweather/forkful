---
title:                "Tests schreiben"
date:                  2024-02-01T22:08:36.886492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Tests in der Programmierung zu schreiben bedeutet, spezifische Verfahren zu erstellen, um die Funktionalität und Leistung Ihrer Codeabschnitte zu validieren und sicherzustellen, dass sie unter verschiedenen Bedingungen wie erwartet funktionieren. Programmierer tun dies, um frühzeitig Fehler zu finden, die Codequalität zu verbessern und die zukünftige Wartung und Erweiterung des Codes zu erleichtern.

## Wie man:

Obwohl Visual Basic for Applications (VBA) nicht mit einem integrierten Testframework wie den in Sprachen wie Python oder JavaScript verfügbaren kommt, können Sie dennoch einfache Testverfahren implementieren, um die Integrität Ihres Codes zu überprüfen. Hier ist ein Beispiel zur Veranschaulichung:

Angenommen, Sie haben eine Funktion in VBA, die zwei Zahlen addiert:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Um diese Funktion zu testen, können Sie ein weiteres Verfahren schreiben, das seine Ausgabe gegen erwartete Ergebnisse validiert:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Test bestanden!", vbInformation
    Else
        MsgBox "Test fehlgeschlagen. Erwartet 15, aber erhalten " & result, vbCritical
    End If
End Sub
```

Das Ausführen von `TestAddNumbers` zeigt eine Nachrichtenbox an, die anzeigt, ob der Test basierend auf der Ausgabe der Funktion bestanden oder fehlgeschlagen ist. Obwohl dies ein vereinfachtes Szenario ist, können Sie komplexere Tests aufbauen, indem Sie Schleifen, verschiedene Eingabewerte und Tests für mehrere Funktionen einbeziehen.

## Tiefere Einblicke

Der hier gezeigte Ansatz zum Schreiben von Tests in VBA ist manuell und verfügt nicht über die Funktionen von ausgefeilteren Testframeworks, die in anderen Programmierumgebungen verfügbar sind, wie automatisierte Testdurchläufe, Setup-/Teardown-Prozeduren und integrierte Berichterstattung über Testergebnisse. Bevor Unit-Testing-Frameworks und Test-Driven Development (TDD) breiter akzeptiert wurden, waren manuelle Testverfahren ähnlich dem beschriebenen üblich. Obwohl diese Methode einfach ist und für kleine Projekte oder Lernzwecke effektiv sein kann, ist sie nicht skalierbar oder effizient für größere Projekte oder Teams.

In Umgebungen, die reichhaltigere Entwicklungswerkzeuge unterstützen, wenden sich Programmierer oft an Frameworks wie NUnit für .NET-Anwendungen oder JUnit für Java-Anwendungen, die umfassende Werkzeuge zum systematischen Schreiben und Ausführen von Tests bieten. Diese Frameworks bieten fortgeschrittene Funktionen wie das Assertieren von Testergebnissen, das Einrichten von Mock-Objekten und das Messen der Codeabdeckung.

Für VBA-Entwickler, die nach fortgeschritteneren Testfähigkeiten suchen, könnte die nächstbeste Alternative darin bestehen, externe Werkzeuge zu nutzen oder sich mit anderen Programmierumgebungen zu integrieren. Einige Entwickler verwenden VBA in Verbindung mit Excel, um Testszenarien und -ergebnisse manuell zu erfassen. Obwohl dies nicht so bequem oder automatisiert wie die Verwendung eines dedizierten Testframeworks ist, können diese Methoden die Lücke teilweise schließen und dabei helfen, die Zuverlässigkeit von VBA-Lösungen in komplexen oder kritischen Anwendungen aufrechtzuerhalten.
