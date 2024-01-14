---
title:                "C#: Lesing av tekstfil"
simple_title:         "Lesing av tekstfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Lesing og behandling av tekstfiler er en viktig del av programmering. Det lar oss lese data fra en fil og bruke den i våre programmer. Det er også nyttig for å behandle store mengder data eller for å lage rapporter basert på tekstfiler.

## Hvordan gjøre det
For å lese en tekstfil i C#, må vi først opprette en instans av klassen `StreamReader` og koble den til filen vi vil lese. Dette gjøres ved å bruke `File.OpenText()`-metoden og oppgi banen til filen som parameter.

```C#
using System; 
using System.IO;

StreamReader reader = File.OpenText("filbane/filnavn.txt"); // oppretter en instans av `StreamReader` og åpner filen
string line = reader.ReadLine(); // leser en linje fra filen og lagrer den i en string
Console.WriteLine(line); // skriver ut linjen i konsollen
reader.Close(); // lukker filen
```

Output:
```
Dette er en linje med tekst i filen.
```

Vi kan også lese en hel fil på en gang ved hjelp av `ReadToEnd()`-metoden, som returnerer hele innholdet i filen som en string. Dette kan være nyttig når vi jobber med mindre filer.

```C#
using System; 
using System.IO;

string fileContent = File.ReadAllText("filbane/filnavn.txt"); // leser hele filen og lagrer innholdet som en string
Console.WriteLine(fileContent); // skriver ut innholdet i filen i konsollen
```

Output:
```
Dette er en linje med tekst i filen.
Dette er en annen linje med tekst.
Enda en linje med tekst.
```

## Dypdykk
Når vi leser en tekstfil, er det viktig å håndtere eventuelle feil som kan oppstå. For eksempel hvis filen ikke eksisterer eller hvis det oppstår en feil under lesingen. Dette kan gjøres ved å bruke `try-catch`-blokker i koden vår.

```C#
try 
{
    StreamReader reader = File.OpenText("filbane/filnavn.txt"); // prøver å åpne filen
    string line = reader.ReadLine();
    Console.WriteLine(line);
    reader.Close(); // hvis filen er åpen, lukker vi den
}
catch (Exception e)
{
    Console.WriteLine(e.Message); // hvis det oppstår en feil, skriver vi ut feilmeldingen
}
```

Vi kan også lese fra og skrive til filer samtidig ved å bruke `StreamWriter`-klassen. Dette kan være nyttig for å opprette nye filer eller legge til innhold i eksisterende filer.

```C#
using System;
using System.IO;

try 
{
    StreamWriter writer = File.AppendText("filbane/filnavn.txt"); // oppretter en instans av `StreamWriter` og åpner filen
    writer.WriteLine("En ny linje skrevet med StreamWriter.");
    writer.Close(); // lukker filen
}
catch (Exception e)
{
    Console.WriteLine(e.Message); // hvis det oppstår en feil, skriver vi ut feilmeldingen
}

StreamReader reader = File.OpenText("filbane/filnavn.txt"); // åpner filen på nytt for å lese den
string line = reader.ReadLine(); // leser den nye linjen vi skrev til filen
Console.WriteLine(line);
reader.Close(); // lukker filen
```

Output:
```
Dette er en linje med tekst i filen.
Dette er en annen linje med tekst.
Enda en linje med tekst.
En ny linje skrevet med StreamWriter.
```

## Se også
- [Microsoft Docs - StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [Microsoft Docs - StreamWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)