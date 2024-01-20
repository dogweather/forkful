---
title:                "Kontrollera om en katalog finns"
html_title:           "C#: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog existerar i programmering innebär att verifiera att en filväg leder till en fysiskt närvarande mapp på en enhet. Programmerare gör detta för att undvika fel och undantag som uppstår när man försöker att läsa från, skriva till, eller interagera med en icke-existerande katalog.

## Hur man gör:

C# ger oss en elegant och robust metod `Directory.Exists` för att verifiera existensen av en katalog. Kodexemplet nedan visar hur man använder den.

```C#
using System.IO;

string path = @"C:\example\directory\path";

if (Directory.Exists(path))
{
    System.Console.WriteLine("Directory exists.");
}
else
{
    System.Console.WriteLine("Directory does not exist.");
}
```

Produktionen av denna kod kommer att vara antingen "Directory exists." eller "Directory does not exist." beroende på status för `'path'`.

## Fördjupning:

`Directory.Exists` i C#, introducerades med .NET Framework 1.1. Alternativen till `Directory.Exists` inkluderar att hantera undantag som uppstår när man interagerar med en icke-existerande katalog, men detta är ingen bra praxis och undvikt bäst. `Directory.Exists` har intern implementering med hjälp av Win32 API-funktionen `GetFileAttributesW` för att säkerställa snabb och effektiv verifikation av katalognärvaro.

## Se även:

1. Microsoft officiella dokumentation om Directory.Exists: [https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
2. Diskussionsforum för C#-utvecklare, där liknande ämnen diskuteras: [https://stackoverflow.com/questions/tagged/c%23](https://stackoverflow.com/questions/tagged/c%23)