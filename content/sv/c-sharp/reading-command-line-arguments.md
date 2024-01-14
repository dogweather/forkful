---
title:    "C#: Läsning av kommandoradsargument"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en viktig färdighet för alla C#-programmerare. Genom att lära sig detta kan du ge dina program mer flexibilitet och anpassningsbarhet. Det låter även användare interagera med ditt program på ett mer djupgående sätt.

## Så här gör du

Att läsa kommandoradsargument kan verka komplicerat, men det är faktiskt väldigt enkelt. I C# finns det två huvudsakliga metoder för att göra detta: med hjälp av `Main()` metodens `args` parameter eller med `Environment.GetCommandLineArgs()` metoden.

### Med `Main()` metodens `args` parameter

För att läsa in kommandoradsargument med `args` parameter behöver du först och främst deklarera det som en parameter i din `Main()` metod, till exempel:

```C#
static void Main(string[] args)
{
    // Din kod här
}
```

När du kör ditt program från kommandoraden kan du nu skicka med olika argument som separeras av mellanslag, till exempel:

```bash
dotnet program.exe argument1 argument2 argument3
```

I din kod kan du sedan komma åt dessa argument genom att använda `args` arrayen, till exempel:

```C#
static void Main(string[] args)
{
    // Skriver ut alla argument som skickades med
    foreach(var arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

Om du exempelvis kör programmet med kommandoradsargumenten "hello world", kommer konsolen att skriva ut "hello" på första raden och "world" på andra raden.

### Med `Environment.GetCommandLineArgs()` metoden

En annan metod för att läsa in kommandoradsargument är att använda `Environment.GetCommandLineArgs()` metoden. Detta är en mer generell och lättanvänd metod som returnerar en array av alla argument som skickades med till ditt program.

```C#
static void Main(string[] args)
{
    // Hämtar alla argument som skickades med
    var commandLineArgs = Environment.GetCommandLineArgs();
    
    // Skriver ut alla argument
    foreach (var arg in commandLineArgs)
    {
        Console.WriteLine(arg);
    }
}
```

Skillnaden mellan denna metod och den första metoden är att denna även inkluderar själva programnamnet som det första argumentet. Så om du kör programmet med samma kommandoradsargument som tidigare, kommer konsolen nu att skriva ut "program.exe" första raden och sedan "hello" och "world" på de följande raderna.

## Djupdykning

Att läsa kommandoradsargument kan hjälpa dig att göra dina program mer anpassningsbara, men det finns vissa saker att tänka på. För det första är det viktigt att ha en tydlig struktur på hur dina argument ska tolkas och användas av ditt program. Det kan även vara en bra idé att göra vissa argument obligatoriska och andra valfria för att undvika problem eller felaktig användning.

En annan sak att tänka på är att kontrollera och hantera eventuella felmeddelanden från inmatade argument, som till exempel ogiltiga tecken eller argument som inte matchar dina förväntningar.

Det finns också möjlighet att använda tredjepartsbibliotek för att underlätta inläsningen av kommandoradsargument, vilket kan vara särskilt användbart för mer komplexa argumentstrukturer.

## Se även

- [Microsofts dokumentation om "Command line arguments in C#"](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Tutorial: Using Command Line Arguments in C#](https://www.tutlane.com/tutorial/csharp/csharp-command-line-arguments-with-examples)
- [Dokumentation för tredjepartsbiblioteket "Command Line Parser"](https://github.com/commandlineparser/commandline)