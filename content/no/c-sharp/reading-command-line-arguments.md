---
title:                "C#: Lesing av kommandolinjeargumenter"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Kommandolinjeargumenter er et viktig konsept innen programmering, spesielt innen C#. Det lar deg sende og motta informasjon når du kjører et program via kommandolinjen. Dette kan være nyttig for å gjøre programmet ditt mer fleksibelt og interaktivt, og å spare tid når du trenger å endre noen innstillinger. Derfor er det viktig å forstå hvordan man leser kommandolinjeargumenter i C#.

# Hvordan

For å lese kommandolinjeargumenter i C#, trenger du å bruke "args" parameteret i metoden "Main". Dette parameteret inneholder en matrise av strenger som inneholder alle argumentene som er gitt via kommandolinjen. Her er et eksempel på hvordan du kan lese og skrive ut argumentene:

'``C#
static void Main(string[] args)
{
    for(int i=0; i<args.Length; i++)
    {
        Console.WriteLine("Argument {0}: {1}", i, args[i]);
    }
}
```

Kjør dette programmet fra kommandolinjen og gi noen argumenter som parametere, for eksempel "dotnet argumenter.exe hei verden". Output vil være:

```
Argument 0: hei
Argument 1: verden
```

Det er også viktig å merke seg at argumentene blir behandlet som strenger, så hvis du for eksempel trenger å konvertere et argument til et tall, må du bruke konverteringsfunksjoner som "int.Parse ()" eller "double.Parse ()".

# Dypdykk

Et viktig konsept å forstå når man leser kommandolinjeargumenter i C# er å forstå rekkefølgen de er gitt i. Argumentene gis alltid i rekkefølgen de er angitt, og de kan heller ikke bli omorganisert når programmet er i gang. Hvis du for eksempel kjører programmet ditt med kommandolinjeargumentene "program.exe hei verden", og deretter bytter rekkefølgen til "program.exe verden hei", vil output fortsatt være:

```
Argument 0: hei
Argument 1: verden
```

Det er også mulig å inkludere flagg eller brytere som ekstra kommandolinjeargumenter. Disse er forhåndsdefinerte stringverdier som indikerer spesifikke handlinger eller innstillinger som programmet skal ta. For å lese disse flaggene, kan du bruke funksjoner som "args.Contains ()" eller "args.IndexOf ()" for å søke gjennom matrisen med argumenter.

# Se også

- [Microsoft documentasjon for å lese kommandolinjeargumenter i C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Tutorialspoint guide til å lese og behandle argumenter i C#](https://www.tutorialspoint.com/csharp/csharp_command_line_arguments.htm)
- [Codeburst forklaring av kommandolinjeargumenter i C#](https://codeburst.io/command-line-arguments-in-c-b16c41586b4e)