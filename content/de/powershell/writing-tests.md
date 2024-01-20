---
title:                "Tests schreiben"
html_title:           "PowerShell: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/writing-tests.md"
---

{{< edit_this_page >}}

# Warum & Was?
Beim Programmieren schreiben Entwickler oft Tests, um die Funktionalität ihrer Programme zu überprüfen. Dadurch können sie sicherstellen, dass das Programm wie erwartet funktioniert und mögliche Fehler frühzeitig erkennen.

# Wie geht's?
In PowerShell können Tests mithilfe des `Pester`-Moduls geschrieben werden. Hier ist ein Beispiel, wie man eine Test-Datei mit dem Namen `TestExample.Tests.ps1` erstellt:
```PowerShell
Describe "Addition" {
    It "adds two numbers correctly" {
        $result = 2+2
        $expectedResult = 4
        $result | Should -Be $expectedResult
    }
}
```
In diesem Beispiel wird getestet, ob die Addition von 2 und 2 das erwartete Ergebnis liefert. Wenn der Test erfolgreich ist, wird keine Ausgabe angezeigt. Falls es einen Fehler gibt, wird dieser in der Konsole angezeigt.

# Tiefere Einblicke
Tests werden bereits seit vielen Jahren in der Softwareentwicklung eingesetzt, um die Qualität von Programmen zu verbessern. Alternativen zu `Pester` sind beispielsweise `NUnit` oder `xUnit`, die auf anderen Programmiersprachen basieren. Bei der Implementierung von Tests ist es wichtig, dass sie einfach zu warten und ausführbar sind.