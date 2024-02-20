---
date: 2024-01-20 18:03:07.778209-07:00
description: "Ein neues Projekt zu starten, bedeutet, dass du in C# von Grund auf\
  \ etwas Eigenes erschaffst. Programmierer tun das, um neue Ideen umzusetzen, Probleme\
  \ zu\u2026"
lastmod: 2024-02-19 22:05:12.816679
model: gpt-4-1106-preview
summary: "Ein neues Projekt zu starten, bedeutet, dass du in C# von Grund auf etwas\
  \ Eigenes erschaffst. Programmierer tun das, um neue Ideen umzusetzen, Probleme\
  \ zu\u2026"
title: Einen neuen Projekt starten
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu starten, bedeutet, dass du in C# von Grund auf etwas Eigenes erschaffst. Programmierer tun das, um neue Ideen umzusetzen, Probleme zu lösen oder um einfach etwas Neues zu lernen.

## How to:
```C#
using System;

namespace MeinErstesProjekt
{
    class Programm
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hallo Welt, mein erstes C# Projekt!");
        }
    }
}
```
Ausgabe:
```
Hallo Welt, mein erstes C# Projekt!
```
Um ein neues Projekt zu starten, öffne deine Entwicklungsumgebung (z.B. Visual Studio), wähle "Neues Projekt erstellen", folge den Anweisungen des Assistenten und kopiere den Code oben in die `Main`-Methode.

## Deep Dive
Ein frisches C#-Projekt zu starten war nicht immer so straightforward. Früher, vor der Entwicklung von integrierten Entwicklungsumgebungen (IDEs) und vor .NET, schrieben Programmierer ihren Code in Texteditoren und kompilierten manuell über die Kommandozeile. Heute kannst du mithilfe von IDEs wie Visual Studio, Visual Studio Code oder JetBrains Rider schnell ein Projekt aufsetzen. Alternativ kannst auch die .NET CLI verwenden (z.B. `dotnet new console`), um Projekte ohne GUI zu initialisieren. Das 'Hello World'-Beispiel oben zeigt den minimalistischen Aufbau eines Konsolenprojekts: Eine `Programm`-Klasse mit einer `Main`-Methode – der Einstiegspunkt jedes C#-Programms.

## See Also
- [Microsoft C# Dokumentation](https://docs.microsoft.com/de-de/dotnet/csharp/)
- [.NET CLI Dokumentation](https://docs.microsoft.com/de-de/dotnet/core/tools/)
- [Visual Studio Download](https://visualstudio.microsoft.com/de/downloads/)
- [JetBrains Rider](https://www.jetbrains.com/rider/)
