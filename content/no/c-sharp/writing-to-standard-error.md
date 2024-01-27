---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til standard error (stderr) handler om å sende feilmeldinger til et egen kanal, adskilt fra normal output (stdout). Programmerere gjør dette for å lettere kunne fange og håndtere feil, samt å logge disse separat for feilsøking.

## Hvordan:
```C#
using System;

class Program
{
    static void Main()
    {
        try
        {
            // Her produserer vi en feil med vilje
            throw new Exception("En feil har skjedd!");
        }
        catch(Exception e)
        {
            // Skriver feilmeldingen til standard error
            Console.Error.WriteLine(e.Message);
        }
    }
}
```
Forventet output til stderr:
```
En feil har skjedd!
```
Merk: Kjøring i en IDE kan vise dette i output-vinduet, men det er faktisk sent til stderr streamen.

## Dykket Ned:
Historisk sett kommer konseptet med stderr fra Unix-systemer der det var viktig å skille mellom normal output (stdout) og feilmeldinger (stderr) for bedre kontroll over scripting og rørlegging (piping). I .NET kan vi bruke `Console.Error` for å nå stderr. Alternativer inkluderer logging biblioteker som NLog eller log4net som tilbyr mer fleksibilitet. Det grunnleggende trekket ved å skrive til stderr i C# er det at det ikke påvirker vanlig output-strømmen, og dermed gjør det mulig for feilmeldinger å bli omdirigert eller logget separat.

## Se Også:
- Microsofts dokumentasjon på Console-klassen: https://docs.microsoft.com/en-us/dotnet/api/system.console
- .NET logging biblioteker: 
  - NLog: https://nlog-project.org/
  - log4net: https://logging.apache.org/log4net/
