---
title:                "Skapa en tillfällig fil"
html_title:           "C#: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När en programmerare skapar ett temporärt fil så skapas en fil som endast existerar temporärt, detta betyder att den inte sparas permanent. Detta görs för att tillfälligt lagra data som sedan kan raderas när det inte längre behövs.

## Hur gör man:
För att skapa en temporär fil i C# används klassen ```System.IO.Path``` tillsammans med metoden ```GetTempFileName()```. Detta returnerar en sträng som representerar en giltig sökväg till den temporära filen.

```C#
string tempFile = Path.GetTempFileName();
File.WriteAllText(tempFile, "Temporär fil!");
Console.WriteLine(File.ReadAllText(tempFile));
```

Detta kodexempel skapar en temporär fil, skriver ut en text till filen och läser sedan in texten från filen.

```C#
Temporär fil!
```

## Djupdykning:
Skapandet av temporära filer har funnits sedan tidigt i datorernas historia och är fortfarande en vanlig teknik för programmerare. Alternativ till att skapa temporära filer är att använda minnet direkt eller att använda en databas, men vissa situationer kräver fortfarande användningen av temporära filer.

Fördelen med att skapa en temporär fil istället för att använda minnet direkt är att filen kan delas mellan processer. Detta är särskilt användbart i flertrådad programmering där flera trådar kan behöva tillgång till filen samtidigt. Detta gör också att det går att läsa och skriva till filen i olika delar av koden utan att behöva oroa sig för att störa andra processer.

## Se även:
- [MSDN - Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- [Creating Temporary Files and Folders in C#](https://www.c-sharpcorner.com/article/creating-temporary-files-and-folders-in-C-Sharp/)
- [Sharing Files with Multiple Processes](https://docs.microsoft.com/en-us/dotnet/standard/io/sharing-files-with-multiple-processes)