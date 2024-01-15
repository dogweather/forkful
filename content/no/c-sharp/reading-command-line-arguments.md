---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "C#: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har brukt et program eller en applikasjon som lar deg legge til argumenter i kommandolinjen, så har du kanskje lurt på hvorfor dette er nødvendig. Å lese kommandolinjeargumenter kan faktisk være utrolig nyttig, da det gir deg muligheten til å tilpasse programmet ditt etter dine egne behov.

# Hvordan

For å kunne lese kommandolinjeargumenter i C#, må du først importere "System" og "System.Collections" namespaces i koden din. Deretter kan du bruke "Environment.GetCommandLineArgs()" metoden til å hente alle argumentene som blir levert til programmet ditt. Se eksempelet nedenfor:

```C#
using System;
using System.Collections;

class Program
{
  static void Main(string[] args)
  {
    string[] arguments = Environment.GetCommandLineArgs();

    if (arguments.Length > 1)
    {
      Console.WriteLine("Første argumentet er: " + arguments[1]);
      Console.WriteLine("Alle argumenter er:");

      for (int i = 0; i < arguments.Length; i++)
      {
        Console.WriteLine(arguments[i]);
      }
    }
  }
}
```

Eksempel på programmet kjørt med argumentene "Hei på deg!" og "12345":

```
Første argumentet er: Hei på deg!
Alle argumenter er:
C:\Program Files\dotnet\dotnet.exe
C:\Users\user\Documents\program.exe
Hei på deg!
12345
```

# Dypdykk

Det finnes flere måter å lese kommandolinjeargumenter på i C#, som å bruke "Environment.CommandLine" for å hente hele kommandolinjen og deretter bruke "string.Split()" metoden for å dele den opp i en array. Fordelen med å lese argumentene på denne måten er at du kan få tilgang til programnavnet og andre informasjon om selve kommandolinjen. Du kan også bruke "args[]" parameteret i "Main()" metoden for å lese argumentene, men dette begrenser deg til å bare lese argumentene som blir levert når du starter programmet og ikke etterpå.

# Se også

- [Environment.GetCommandLineArgs() metoden dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs)
- [Environment.CommandLine egenskap dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.environment.commandline)
- [Split() metoden dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.string.split)