---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, Code zu kreieren, der deinen Code überprüft. Es sichert Qualität und erspart später Kopfschmerzen, indem Fehler früh erkannt werden.

## How to:
Zum Testen verwende das NUnit Framework. Installiere es über NuGet, und schreib dann folgenden Test:

```C#
using NUnit.Framework;

namespace BeispielTests
{
    public class EinfacheTests
    {
        [Test]
        public void Addieren_ZweiPlusZwei_ErgibtVier()
        {
            // Arrange
            var erwartetesErgebnis = 4;

            // Act
            var ergebnis = Addieren(2, 2);

            // Assert
            Assert.AreEqual(erwartetesErgebnis, ergebnis);
        }

        int Addieren(int a,int b)
        {
            return a + b;
        }
    }
}
```

Führe den Test aus. Resultat:

```
Test Passed: Addieren_ZweiPlusZwei_ErgibtVier
```

## Deep Dive:
Tests begannen mit dem Aufkommen von TDD (Test Driven Development) um die Jahrtausendwende an Popularität zu gewinnen. Alternativen zu NUnit sind MSTest und xUnit, die ähnlich funktionieren. Beim Testen ist wichtig, kleine und isolierte Testfälle zu schreiben, um spezifische Funktionen zu prüfen.

## See Also:
- [NUnit Documentation](https://nunit.org/docs/)
- [Microsofts xUnit-Testframework](https://xunit.net/)
- [Test Driven Development (TDD) auf Wikipedia](https://de.wikipedia.org/wiki/Testgetriebene_Entwicklung)
