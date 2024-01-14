---
title:                "C#: Att skriva en textfil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför?

Att skriva textfiler är ett viktigt koncept inom programmering, speciellt inom C#. Textfiler används för att lagra och hantera data som kan vara viktig för ett program. Genom att lära sig hur man skriver en textfil kan man lätt manipulera och spara data för senare användning.

## Så här gör du:

För att skriva en textfil i C#, behöver vi först öppna en filström och ange sökvägen för den textfil vi vill skapa. Sedan använder vi StreamWriter-klassen för att skriva till filen och stänga filströmmen efteråt. Här är ett exempel på hur koden kan se ut:

```C#
// Skapa en filström och ange sökvägen till textfilen
FileStream fs = new FileStream("mittDokument.txt", FileMode.Create);

// Skapa en StreamWriter för att skriva till filen
StreamWriter sw = new StreamWriter(fs);

// Skriv till filen
sw.WriteLine("Det här är en text som kommer att sparas i mittDokument.txt");
sw.WriteLine("Jag kan skriva flera rader och också använda olika datatyper som int och double.");

// Stäng StreamWriter och filströmmen
sw.Close();
fs.Close();
```

När du kör koden ovan kommer det skapas en fil med namnet "mittDokument.txt" och texten som angavs i koden kommer att skrivas till filen. Resultatet kommer att se ut så här:

> Det här är en text som kommer att sparas i mittDokument.txt
> Jag kan skriva flera rader och också använda olika datatyper som int och double.

## Djupdykning:

Nu när vi vet hur vi kan skriva till en textfil, låt oss utforska några andra användbara funktioner som StreamWriter-klassen har.

En av dessa funktioner är att kunna ange om filen ska fortsätta skrivas på eller om den ska skrivas över varje gång vi öppnar den. Detta kan göras genom att ange FileMode.Append istället för FileMode.Create i filströmmen.

En annan användbar funktion är att kunna formatera texten som skrivs till filen. Detta görs genom att använda Format eller WriteFormat-metoderna istället för Write-metoden.

Nu när du har en grundläggande förståelse för hur man skriver en textfil i C#, kan du utforska mer avancerade koncept som att läsa från och skriva till befintliga textfiler, eller hantera fel och undantag som kan uppstå. Det finns mycket att lära om textfiler och det är en viktig del av programmering.

## Se även:

- [MSDN dokumentation för StreamWriter-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [Skillnaden mellan FileMode.Append och FileMode.Create](https://stackoverflow.com/questions/757310/whats-the-difference-between-filemode-create-and-filemode-append)
- [Sköldpaddans beteende: Läs in data från en textfil](https://www.skoldpaddansbloggen.se/?p=1013)