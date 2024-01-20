---
title:                "Sjekker om en mappe eksisterer"
html_title:           "C#: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kontroll for om en mappe eksisterer er en essensiell funksjon for programmering. Det lar programmerere sikre at en spesifisert sti er gyldig, forhindre feil og bedre brukeropplevelsen.

## Hvordan:

Bruk Directory.Exists funksjonen i System.IO namespace. Her er hvordan du gjør det:

```C#
using System.IO;

class Program 
{
    static void Main()
    {
        string path = @"C:\temp";

        if (Directory.Exists(path))
        {
            System.Console.WriteLine("Mappen eksisterer.");
        }
        else
        {
            System.Console.WriteLine("Mappen eksisterer ikke.");
        }
    }
}
```

Output vil være en av følgende, avhengig av om mappen eksisterer:

``` 
Mappen eksisterer.
```
Eller

``` 
Mappen eksisterer ikke.
```

## Dyp Dykk

Metoden for å sjekke om en mappe eksisterer har vært en del av C# siden det ble introdusert, men det er viktig å merke seg at det aldri er en 100% pålitelig måte å sjekke. En ny mappe kan bli opprettet nettopp etter at sjekken ble utført, eller tilgangsrettigheter kan endre seg.

Alternativt kan du også bruke DirectoryInfo.Exists metoden eller prøve å åpne en fil i mappen og fange unntaket hvis det mislykkes. Disse metodene er dyrere i form av ytelse, men kan gi mer presis informasjon i noen tilfeller.

En detalj om implementasjonen av Directory.Exists er at det faktisk ikke åpner mappen. Det bruker et operativsystemkall for å hente informasjon om mappen, så det er relativt raskt og bruker lite ressurser.

## Se Også

[Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)

[DirectoryInfo.Exists Property](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo.exists?view=net-5.0)

[How to: Check that a File or Folder Exists](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-check-that-a-file-or-folder-exists)