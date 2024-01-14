---
title:    "C#: Tests schreiben"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Tests schreiben ist ein wichtiger Bestandteil der Softwareentwicklung. Sie helfen dabei, Fehler frühzeitig zu erkennen und zu vermeiden, und sorgen für eine höhere Codequalität. Ohne Tests besteht die Gefahr, immer wieder dieselben Fehler zu machen und somit mehr Zeit und Mühe in die Fehlerbehebung zu investieren. In diesem Blogbeitrag werde ich Ihnen zeigen, wie Sie in C# Tests schreiben können und warum es eine wichtige Praxis für Entwickler ist.

## Wie man Tests in C# schreibt
Um mit dem Schreiben von Tests in C# zu beginnen, benötigen Sie ein Test-Framework wie z.B. NUnit oder xUnit. Diese Frameworks stellen eine Vielzahl von nützlichen Funktionen bereit, um Tests zu erstellen und auszuführen.

Beginnen wir mit einem einfachen Beispiel eines C#-Tests mit NUnit:
```C#
[TestFixture]
public class CalculatorTests
{
  [Test]
  public void Add_AddsTwoNumbers_ReturnsSum()
  {
    // Arrange
    Calculator calculator = new Calculator();

    // Act
    int result = calculator.Add(2, 3);

    // Assert
    Assert.AreEqual(5, result);
  }
}
```
Dieser Test prüft die Funktionalität der Add-Methode in unserer Calculator-Klasse, indem er zwei Zahlen addiert und sicherstellt, dass das Ergebnis korrekt ist. Zuerst definieren wir eine Testklasse mit dem Attribut `[TestFixture]`, um anzuzeigen, dass es sich um eine Testsammlung handelt. Innerhalb dieser Klasse können wir einzelne Tests mit dem `[Test]`-Attribut erstellen. In diesem Fall stellen wir sicher, dass das Ergebnis des Aufrufs von `Add(2, 3)` gleich 5 ist.

Natürlich gibt es noch viel mehr, was man mit Test-Frameworks wie NUnit und xUnit tun kann. Sie bieten viele weitere Attribute, Assertions und Möglichkeiten zur Organisierung von Tests. Eine ausführlichere Anleitung finden Sie in den See Also-Links am Ende dieses Beitrags.

## Deep Dive
Beim Schreiben von Tests ist es wichtig, sich an bestimmte Prinzipien zu halten, um eine effektive Test-Suite zu erstellen. Ein wichtiger Punkt ist, dass Tests unabhängig voneinander sein sollten. Das bedeutet, dass jeder Test für sich allein funktionieren sollte und nicht von anderen Tests abhängig sein darf. Außerdem ist es wichtig, ausreichend Tests zu schreiben, um alle möglichen Fälle abzudecken und eventuelle Fehler aufzudecken.

Ein weiteres nützliches Konzept beim Schreiben von Tests ist das Test-Driven Development (TDD). Dabei schreibt man zuerst den Test für eine gewünschte Funktionalität und erst dann den Code, um den Test zu erfüllen. Diese Vorgehensweise kann dazu beitragen, besseren und stabileren Code zu schreiben.

Zudem sollten Tests regelmäßig ausgeführt werden, z.B. bei jedem Build oder Push in ein Versionskontrollsystem. Dadurch wird sichergestellt, dass eventuelle Änderungen den Code nicht brechen.

## Siehe auch
- [NUnit Dokumentation](https://docs.nunit.org/)
- [xUnit Dokumentation](https://xunit.net/)

Danke, dass Sie meinen Blogbeitrag zum Thema Testen in C# gelesen haben. Ich hoffe, er hat Ihnen gezeigt, wie wichtig es ist, Tests zu schreiben und wie man mit Test-Frameworks wie NUnit und xUnit effektiv Tests erstellen kann. Vergessen Sie nicht, immer zu testen und Ihre Tests regelmäßig auszuführen, um eine bessere Codequalität zu erreichen. Viel Spaß beim Coden!