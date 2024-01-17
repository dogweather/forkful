---
title:                "Skrivande till standardfel"
html_title:           "C#: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Skrivning till standardfel är en programmeringspraxis som innebär att skriva ut fel och andra meddelanden till standardfelströmmen istället för standardutströmmen. Detta gör det möjligt för oss att enkelt separera felmeddelanden från normal utdata. Programmerare använder denna teknik för att underlätta felsökning och förbättra programkvaliteten.

# Hur man gör:

För att skriva till standardfel i C# använder vi Console.Error.WriteLine() metoden. Här är ett exempel på hur det skulle se ut:

```C#
Console.Error.WriteLine("Ett fel har uppstått.");
```

Detta kommer att skriva ut meddelandet "Ett fel har uppstått." till standardfelströmmen. Om du vill skriva ut mer detaljerad information om felet kan du använda Console.Error.Write() metoden istället.

# Djupdykning:

Skrivning till standardfelströmmen är en standardfunktion inom C# sedan version 2.0. Det ger en enkel och effektiv metod för att skriva ut fel och andra meddelanden utan att behöva använda externa bibliotek eller verktyg. Alternativet till att skriva till standardfel är att använda en extern loggningsramverk, men detta kan medföra en ökad komplexitet och prestandaförlust.

Implementeringen av skrivning till standardfel görs genom att ändra standardfelströmmen till standardutströmmen. Detta kan då skrivas ut som vanligt med Console.Write() eller Console.WriteLine() metoden. Det finns även möjlighet att återställa standardfelströmmen till standardutströmmen, vilket skulle låta programmet skriva till båda strömmarna.

# Se även:

- [Console.Error.WriteLine() - Microsoft Docs](https://docs.microsoft.com/sv-se/dotnet/api/system.console.error.writeline?view=netcore-3.1)
- [Logging Frameworks in C# - C# Corner](https://www.c-sharpcorner.com/UploadFile/9582c9/logging-frameworks-in-C-Sharp/)
- [C# Console Class - W3Schools](https://www.w3schools.com/cs/cs_console.asp)