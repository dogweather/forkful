---
title:                "Kontrollera om en mapp finns"
html_title:           "C#: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar betyder helt enkelt att man tittar på en specifik plats i datorn och ser om det finns en mapp med det namnet som man anger. Detta är en vanlig uppgift för programmerare eftersom det kan vara viktigt att veta om en mapp existerar innan man försöker använda den i sitt program.

## Så här gör du:
```C#
if (Directory.Exists(path))
{
    Console.WriteLine("Mappen existerar!");
}
else
{
    Console.WriteLine("Mappen existerar inte!");
}
```
I detta exempel visar vi hur man med en enkel if-sats kan kontrollera om en mapp existerar på en specifik sökväg (path) och sedan skriver ut ett meddelande beroende på om mappen existerar eller inte.

## Djupdykning:
Historiskt sett har det funnits olika sätt att kontrollera om en mapp existerar i C#, men sedan .NET Framework 2.0 introducerades så har Directory-klassen funnits tillgänglig och det har blivit det vanligaste sättet att göra detta.

Ett alternativ till att använda Directory-klassen är att använda metoden File.Exists(), som kan ta både en fil- och mapp-sökväg som argument. Detta kan vara användbart om man vill kontrollera både filer och mappar i sitt program.

När det kommer till implementation kan man även använda sig av try-catch-block för att hantera eventuella fel som kan uppstå när man kontrollerar om en mapp existerar. Det finns även andra sätt att implementera detta, men det viktigaste är att använda en pålitlig och effektiv metod för att kontrollera om en mapp faktiskt existerar.

## Se även:
- [Directory-klassen i .NET documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=net-5.0)
- [File.Exists() metoden i .NET documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.exists?view=net-5.0)