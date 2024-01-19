---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starta ett Nytt Projekt i C#: Ett Basisk Guide

## Vad & Varför?

Att starta ett nytt projekt innebär att skapa ett rent och till kommerligt arbetsutrymme för att utveckla något nytt. Programmerare gör detta för att organisera koden effektivt och underlätta underhåll.

## Hur:

Det är enkelt att skapa ett nytt projekt i C #. Först startar du programmet Visual Studio. Följ sedan dessa instruktioner:

```C#
// Öppna Visual Studio
// Klicka på "File" i övre vänstra hörnet
// Klicka på "New" och sedan "Project"
// Välj "C #" i den vänstra menyn, välj sedan "Console App (.NET Core)"
// Skriv in projektnamnet och klicka på "Create"
```

Då skapar Visual Studio ett nytt projekt med en grundläggande "Hello World" program.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Hello World");
    }
}
```

När du kör detta program, kommer följande visas:

```
Hello World
```

## Djup Dykning

Historiskt sett, innan integrerade utvecklingsmiljöer (IDE) som Visual Studio, var det komplicerat att starta ett nytt projekt. Nu är det en snabb och systematiserad process.

Alternativ till Visual Studio finns, som .NET Core CLI för kommandoradsinterfacet(ok, ett litet ord) och Jetbrains Rider. Dock är Visual Studio det vanligaste verktyget för C # projekt.

När det gäller detaljer, när du startar ett nytt projekt, skapar Visual Studio ett antal filer och mappar. Dessa inkluderar en mapp för ditt projekt, en lösning (`.sln`) fil och en C # (`Program.cs`) fil med en basisk programmetod (Main).

## Se Även

- [Microsoft Visual Studio dokumentation](https://docs.microsoft.com/sv-se/visualstudio/?view=vs-2019)
- [.NET Core CLI manual](https://dotnet.microsoft.com/download/dotnet-core)
- [JetBrains Rider Documentation](https://www.jetbrains.com/help/rider/)