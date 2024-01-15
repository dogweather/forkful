---
title:                "Å sjekke om en mappe eksisterer"
html_title:           "C#: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig for programvareutviklere å sjekke om en mappe eksisterer før de utfører handlinger som å lese eller skrive filer i den. Dette sikrer at programmet ikke krasjer på grunn av å prøve å aksessere en ikke-eksisterende mappe.

## Hvordan gjøre det

Det er enkelt å sjekke om en mappe eksisterer i C# ved hjelp av File-klassen og dens metoder. Her er et eksempel på kode som viser hvordan du kan sjekke om en gitt mappe eksisterer:

```C#
string mappenavn = "C:/MinMappe";

// Sjekk om mappen eksisterer
if (Directory.Exists(mappenavn))
{
    Console.WriteLine("Mappen eksisterer!");
}
else
{
    Console.WriteLine("Mappen eksisterer ikke.");
}
```

Output av dette eksempelet vil være "Mappen eksisterer!" dersom C:/MinMappe faktisk eksisterer, eller "Mappen eksisterer ikke." hvis den ikke gjør det.

## Dypdykk

Det finnes også andre måter å sjekke for eksistensen av en mappe på i C#, som for eksempel å bruke Path-klassen til å bygge en filbane og sjekke om den eksisterer ved hjelp av File.Exists() metoden. Det er viktig å huske på at både Directory.Exists() og File.Exists() metoder returnerer en boolsk verdi (true eller false), og bør derfor brukes i en if-else-uttalelse som vist i eksempelet over.

En annen ting å huske på er at disse metodene bare sjekker om en mappe eksisterer, men ikke om du har tilgang til den. Det er fortsatt viktig å håndtere eventuelle tillatelser eller unntak dersom man prøver å aksessere filer eller mapper i programmet.

## Se også

- [Microsofts offisielle dokumentasjon om å sjekke om en mappe eksisterer i C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [En guide til å håndtere filer og mapper i C#](https://www.c-sharpcorner.com/UploadFile/mahakgupta/files-and-folders-in-C-Sharp/)
- [Tutorial om tillatelser og håndtering av unntak i C#](https://www.tutorialsteacher.com/csharp/csharp-file-io)