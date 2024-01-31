---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel (stderr) används för att separera felmeddelanden från vanlig utdata. Detta gör det lättare för användare och andra program att skilja på resultatdata och felloggning.

## Steg för steg
```C#
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("Ett fel inträffade!");
        Console.WriteLine("Detta är vanlig utdata.");
    }
}
```
Kör du koden ovan kommer "Ett fel inträffade!" att skrivas till stderr och "Detta är vanlig utdata." till stdout. Prova att omdirigera dem i ett kommandofönster för att se skillnaden:

```shell
dotnet run 1> out.txt 2> err.txt
```

## Fördjupning
Stderr dök först upp i Unix och används för att separera olika typer av utdata. Alternativ till att skriva till stderr inkluderar att logga till filer eller använda loggföring bibliotek som log4net. Implementationsmässigt hanterar `Console.Error` i C# det som ett `TextWriter`-objekt vilket gör det lätt att skriva till.

## Se även
- Microsofts docs om `Console.Error`: [Console.Error](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- Förstå stdout och stderr: [Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- log4net, ett loggföringsbibliotek: [log4net in Apache Logging Services](https://logging.apache.org/log4net/)
