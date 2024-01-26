---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:55:26.544284-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Lektüre von Kommandozeilenargumenten ermöglicht es, dass ein C#-Programm beim Start Zusatzinformationen erhält. So passen Programmierer das Verhalten der Anwendung dynamisch an, ohne den Code zu ändern.

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
