---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kommandoradsargument läses in för att skicka data till ett program vid körning. Detta gör programmering flexibelt och anpassningsbart till olika användningssituationer.

## Hur gör man:

```C#
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        Console.WriteLine("Argument: " + arg);
    }
}
```

Om du kör programmet med argumenten `hej värld`, kommer output att vara:

```text
Argument: hej
Argument: värld
```

## Djupt Dyk

(1) Historiskt kontext: Kommandoradsargument har använts i decennier, återifrån tiden då datorns gränssnitt inte var grafiska utan textbaserade.

(2) Alternativ: Ett alternativ till kommandoradsargument är att använda filinmatning och -utdata, men detta kan vara långsammare och kräver mer kod.

(3) Implementeringsdetaljer: I C# samlas kommandoradsargument i en array av strängar som skickas till `Main`-metoden. Arguments index bestämmer ordningen på kommandoradsargument.

## Se Även

För mer information om att använda kommandoradsargument i C#, se följande länkar: 

- Microsoft's [Command-Line Arguments Tutorial](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- En StackOverflow-diskussion om [Command Line Arguments in C#](https://stackoverflow.com/questions/490570/command-line-arguments-in-c-sharp)
- Artikel på GeeksforGeeks: [Command Line Parameters in C#](https://www.geeksforgeeks.org/command-line-parameters-in-c-sharp/)