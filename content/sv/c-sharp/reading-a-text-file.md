---
title:                "C#: Läsning av en textfil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

Varför: Att läsa en textfil är en grundläggande och viktig del av programmering. Det ger möjlighet att läsa och manipulera data från externa källor, vilket är viktigt för många applikationer, särskilt när det gäller att hantera stora mängder data.

Hur man gör det:Det finns flera sätt att läsa en textfil i C#, men den enklaste metoden är att använda klassen StreamReader. Här är ett exempel på hur man öppnar, läser och stänger en textfil:

```C#
using System;
using System.IO;
namespace LäsaTextfil
{
    class Program
    {
        static void Main(string[] args)
        {
            //öppna filen
            StreamReader fil = new StreamReader("textfil.txt");
            //läs en rad i taget tills filen är slut
            while (!fil.EndOfStream)
            {
                string rad = fil.ReadLine();
                Console.WriteLine(rad);
            }
            //stäng filen
            fil.Close();
        }
    }
}
```

Det här exemplet öppnar en textfil med namnet "textfil.txt" och använder en `while`-loop för att läsa en rad i taget tills filen är slut. Slutligen stängs filen för att frigöra resurser. 

Om vår "textfil.txt" innehåller följande text:

```
Detta är en sample textfil.
Här kan man lägga till vad som helst.
```

Så kommer vår kod att producera följande utmatning:

```
Detta är en sample textfil.
Här kan man lägga till vad som helst.
```

Lite djupare: Vid läsning av en textfil finns det några saker att tänka på för att undvika problem. Till exempel kan det finnas specialtecken eller skillnader i teckenkodning som kan påverka hur filen läses in. Det är också viktigt att stänga filen när den inte längre behövs för att undvika potentiella läckor av minne och andra problem.

Se även: 
- [MSDN - StreamReader Class](https://docs.microsoft.com/sv-se/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# File Handling](https://www.tutorialspoint.com/csharp/csharp_file_handling.htm)
- [Reading and Writing Files in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/reading-and-writing-files-in-C-Sharp/)