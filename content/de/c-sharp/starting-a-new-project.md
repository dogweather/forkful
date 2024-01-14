---
title:                "C#: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

##Warum

Es gibt viele Gründe, ein neues Projekt zu starten. Vielleicht wollen Sie Ihre Programmierfähigkeiten verbessern oder einfach eine neue Herausforderung angehen. Egal aus welchem Grund, das Erstellen eines neuen Projekts kann sehr lohnend sein.

##Wie startet man ein neues Projekt mit C#?

Um ein neues Projekt mit C# zu starten, müssen Sie zunächst eine integrierte Entwicklungsumgebung (IDE) wie Visual Studio herunterladen. Sobald Sie die IDE installiert haben, können Sie ein neues Projekt erstellen und mit dem Codieren beginnen.

Hier ist ein Beispiel für das Erstellen einer einfachen "Hallo Welt"-Anwendung:

```C#
using System;
namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hallo Welt!");
        }
    }
}
```

Wenn Sie dieses Programm ausführen, sollten Sie die Ausgabe "Hallo Welt!" sehen.

##Tiefergehende Informationen über das Starten eines neuen Projekts

Bevor Sie mit dem Codieren beginnen, ist es wichtig, eine klare Vorstellung davon zu haben, was Sie mit Ihrem neuen Projekt erreichen wollen. Definieren Sie die Ziele und Funktionen Ihres Projekts, um sicherzustellen, dass Sie auf dem richtigen Weg bleiben.

Eine weitere wichtige Überlegung ist die Struktur Ihres Projekts. Verwenden Sie möglicherweise Patterns oder Prinzipien wie MVC (Model-View-Controller) oder SOLID (Single-Responsibility-Principle, Open-Closed-Principle, Liskov-Substitution-Principle, Interface-Segregation-Principle, Dependency-Inversion-Principle), um eine gut organisierte und leicht wartbare Codebasis zu schaffen.

##Siehe auch

- Visual Studio herunterladen: https://visualstudio.microsoft.com/de/
- Einführung in C#: https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/
- Einführung in die Objektorientierte Programmierung mit C#: https://docs.microsoft.com/de-de/dotnet/csharp/tutorials/intro-to-csharp/