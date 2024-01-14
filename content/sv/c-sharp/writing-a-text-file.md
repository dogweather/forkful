---
title:    "C#: Skriva en textfil"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en vanlig uppgift för många programmersare och är ett viktigt koncept att lära sig inom C#-programmering. Det ger en strukturerad och anpassningsbar lösning för att lagra och hantera data på ett enkelt sätt.

## Hur man gör det

```C#
// Skapa en fil på skrivbordet
string path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
string fileName = "MittTextDokument.txt";
string filePath = Path.Combine(path, fileName);

// Skapa en ny textfil
using (StreamWriter sw = File.CreateText(filePath))
{
    sw.WriteLine("Det här är en textfil som skapades i C#!");
}

// Läsa från textfilen
using (StreamReader sr = File.OpenText(filePath))
{
    string text = sr.ReadToEnd();
    Console.WriteLine(text);
} 
```

Output:
```
Det här är en textfil som skapades i C#!
```

## Djupdykning

När vi skapar en textfil i C#, använder vi klassen `FileStream` för att öppna en strömmen av data till filen och klassen `StreamWriter` för att skriva till filen. Vi använder också `Path`-klassen för att skapa en unik filväg och kombinera den med filnamnet. Genom att använda en `using`-sats ser vi till att alla resurser stängs automatiskt, vilket underlättar för effektiv kodning.

Andra användbara funktioner när det kommer till textfiler är att kunna ange olika teckenkodningar, till exempel UTF-8 eller Unicode. Detta kan göras genom att specificera en annan parameter när du skapar din `StreamWriter`. Du kan också använda `StreamReader`-klassen för att läsa från textfilen istället för att skriva till den.

## Se även

- [C# Filhantering](https://www.w3schools.com/cs/cs_file_handling.asp)
- [Microsoft Docs om Textfiler i C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-open-and-append-to-a-log-file)