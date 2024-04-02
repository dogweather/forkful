---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:09.949230-07:00
description: "Das Schreiben von Tests in C# beinhaltet das Erstellen von automatisierten\
  \ Skripten, um die Funktionalit\xE4t Ihres Codes zu validieren und sicherzustellen,\u2026"
lastmod: '2024-03-13T22:44:53.891662-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben von Tests in C# beinhaltet das Erstellen von automatisierten\
  \ Skripten, um die Funktionalit\xE4t Ihres Codes zu validieren und sicherzustellen,\u2026"
title: Tests Schreiben
weight: 36
---

## Was & Warum?

Das Schreiben von Tests in C# beinhaltet das Erstellen von automatisierten Skripten, um die Funktionalität Ihres Codes zu validieren und sicherzustellen, dass er sich wie erwartet verhält. Programmierer tun dies, um frühzeitig Fehler zu erkennen, die Code-Refaktorisierung zu erleichtern und sicherzustellen, dass neue Änderungen bestehende Funktionen nicht unterbrechen, wodurch die Softwarequalität und -zuverlässigkeit gesteigert wird.

## Wie:

C#-Entwickler verwenden hauptsächlich die Frameworks NUnit oder xUnit zum Schreiben von Tests aufgrund ihrer Flexibilität und des umfangreichen Funktionsumfangs. Hier ist ein einfaches Beispiel, das NUnit verwendet, um eine einfache Additionsfunktion zu testen:

1. **Installieren Sie NUnit und NUnit3TestAdapter** über den NuGet-Paketmanager oder die .NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Erstellen Sie ein C#-Klassenbibliotheksprojekt**, falls Sie dies noch nicht getan haben.

3. **Schreiben Sie eine einfache Funktion** zum Testen. Zum Beispiel eine Additions-Methode in einer Klasse namens `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Schreiben Sie eine Testklasse** mit NUnit:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_ZweiGanzzahlenAddieren_GibtRichtigeSummeZurück()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Führen Sie den Test aus** mit der Testumgebung Ihrer IDE oder der .NET CLI:
```powershell
dotnet test
```

### Beispiel-Ausgabe:

Wenn Ihr Test erfolgreich ist, sollten Sie eine Ausgabe ähnlich dieser sehen:
```
Testlauf erfolgreich.
Gesamttests: 1
     Bestanden: 1
 Gesamtzeit: 1.2345 Sekunden
```

### Verwendung von xUnit:

Wenn Sie xUnit bevorzugen, ähnelt das Setup dem von NUnit. So würden Sie das Testbeispiel für die Klasse `Calculator` mit xUnit umschreiben:

1. **Installieren Sie xUnit und xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Schreiben Sie eine Testklasse mit xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_ZweiGanzzahlenAddieren_GibtRichtigeSummeZurück()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Führen Sie den Test mit der .NET CLI aus** oder mit dem integrierten Testrunner Ihrer IDE.

Sowohl NUnit als auch xUnit bieten leistungsstarke Funktionen für parametrisierte Tests, Setup-/Teardown-Operationen und die Organisation von Tests in Kategorien, was sie zu unverzichtbaren Werkzeugen im Toolkit des C#-Programmierers für die Sicherstellung der Codequalität und Funktionalität macht.
