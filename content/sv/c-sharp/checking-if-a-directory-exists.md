---
title:                "C#: Kontrollera om en katalog existerar"
simple_title:         "Kontrollera om en katalog existerar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Välkommen till mitt nya C# programmarblogg! I denna bloggpost kommer vi att utforska hur man kontrollerar om en mapp existerar i C#. Detta är en grundläggande men viktig funktion som kan hjälpa dig att skapa mer robusta program. Så låt oss dyka in!

## Varför

Innan vi går in i kodexemplen, låt oss prata om varför det är viktigt att kunna kontrollera om en mapp existerar. Det finns många tillfällen när detta kan vara användbart, till exempel när du behöver skapa en ny mapp för att spara filer i ditt program eller när du behöver bekräfta om en specifik mapp redan finns innan du fortsätter med en operation.

## Såhär

För att kontrollera om en mapp existerar i C#, använder vi funktionen `Directory.Exists()` som tillhör klassen `System.IO`. Vi behöver först importera detta namespace till vår kod med hjälp av nyckelordet `using`:

```C#
using System.IO;
```

Sedan är det bara att använda funktionen `Directory.Exists()` och ge den sökvägen till den mapp du vill kontrollera. Till exempel, om vi vill kontrollera om mappen "Dokument" finns i vår hemmapp, skulle vår kod se ut så här:

```C#
if (Directory.Exists(@"C:\Users\Namn\Dokument"))
{
    Console.WriteLine("Mappen finns!");
}
else
{
    Console.WriteLine("Mappen finns inte.");
}
```

Detta kommer att skriva ut antingen "Mappen finns!" eller "Mappen finns inte." beroende på om mappen "Dokument" finns eller inte.

För att göra det mer modulärt, kan vi även lagra resultatet av denna funktion i en boolean-variabel och använda den senare i vår kod. Till exempel:

```C#
bool finnsMapp = Directory.Exists(@"C:\Users\Namn\Dokument");

if (finnsMapp)
{
    Console.WriteLine("Mappen finns!");
}
else
{
    Console.WriteLine("Mappen finns inte.");
}
```

## Djupdykning

Nu när vi har en grundläggande förståelse för hur man kontrollerar om en mapp existerar i C#, låt oss titta på några viktiga saker att tänka på när man använder denna funktion.

För det första, kom ihåg att sökvägen vi anger till funktionen `Directory.Exists()` behöver vara en absolut sökväg, inte en relativ. Detta betyder att det måste inkludera hela sökvägen från rotmappen, till exempel `C:\Users\Namn\Dokument`.

För det andra, kom ihåg att resultaten av denna funktion är beroende av användarrättigheter. Om användaren som kör programmet inte har tillåtelse att läsa eller lista innehållet i mappen vi kontrollerar, kommer resultatet alltid att vara "Mappen finns inte."

Slutligen, om du behöver skapa den här mappen om den inte existerar, kan du använda funktionen `Directory.CreateDirectory()` efter att ha kontrollerat om mappen existerar. Detta kommer att skapa mappen om den inte finns och kasta ett undantag om mappen redan existerar.

## Se också

Här är några användbara länkar för att fortsätta lära dig om arbete med filsystemet i C#:

- [Microsoft Docs: Hur man: Arbeta med fil och mappar](https://docs.microsoft.com/sv-se/dotnet/standard/io/how-to-work-with-files-and-folders)
- [C# Station: Arbete med filer i C#](https://csharp-station.com/Tutorial/CSharp/Lesson16)
- [C# Corner: Filhantering i C#](https://www.c-sharpcorner.com/UploadFile/mahesh/folder-directory-operations-in-C-Sharp/)

Tack för att du läste min bloggpost! Om du har några frågor eller feedback är du välkommen att lämna en kommentar nedan. Glöm inte att följa mig för fler programmeringsrelaterade inlägg på min blogg. Vi ses snart igen!