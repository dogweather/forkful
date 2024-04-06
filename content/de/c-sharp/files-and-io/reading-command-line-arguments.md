---
date: 2024-01-20 17:55:26.544284-07:00
description: "Los geht's: Ausf\xFChrungsbeispiel."
lastmod: '2024-04-05T21:53:55.787272-06:00'
model: gpt-4-1106-preview
summary: "Ausf\xFChrungsbeispiel."
title: Lesen von Kommandozeilenargumenten
weight: 23
---

## Los geht's:
```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        foreach (string arg in args)
        {
            Console.WriteLine($"Argument: {arg}");
        }
    }
}
```
Ausführungsbeispiel:
```
> myapp.exe Hallo Welt
Argument: Hallo
Argument: Welt
```

## Tiefgang:
Die Verarbeitung von Kommandozeilenargumenten stammt aus den Tagen der Text-basierten Benutzerschnittstellen und ist heute noch relevant für Skripte oder Konsolenanwendungen. Eine Alternative ist die Nutzung von Konfigurationsdateien, Umgebungsvariablen oder Benutzeroberflächen für die Eingabe. Wesentlich in C# ist die `string[] args` im `Main()`-Methode, wodurch Argumente als Array von Strings übergeben werden. Aufmerksamkeit erfordert die Sicherheit, besonders bei der Verarbeitung von ungeprüften Eingaben.

## Siehe auch:
- Microsoft-Dokumentation zu `Main()` und Kommandozeilenargumenten: [docs.microsoft.com](https://docs.microsoft.com/dotnet/csharp/programming-guide/main-and-command-args/)
- Artikel über Sicherheitsaspekte: [owasp.org](https://owasp.org/www-project-top-ten/)
- Ein Guide zur argparse-Bibliothek für komplexe Argument-Verarbeitung: [CommandLineParser auf GitHub](https://github.com/commandlineparser/commandline)
