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

Hva & Hvorfor?

 Å sjekke om en mappe eksisterer er en viktig del av programmeringsprosessen. Dette gjør det mulig for programmet å navigere og utføre handlinger i spesifikke mapper på datamaskinen. Å sjekke om en mappe eksisterer er også en måte å sikre at koden kjører uten å krasje, noe som sparer programmører for tid og frustrasjon.

Hvordan:

For å sjekke om en mappe eksisterer i C#, bruker vi Directory.Exists() metoden. Dette returnerer en boolsk verdi (true eller false) avhengig av om mappen finnes eller ikke. Se nedenfor for et eksempel på å sjekke om mappen "Dokumenter" finnes:

```C#
string path = @"C:\Users\Navn\Dokumenter";
if (Directory.Exists(path))
{
    Console.WriteLine("Mappen finnes!");
}
else
{
    Console.WriteLine("Mappen finnes ikke.");
}

// Output: Mappen finnes!
```

Hvis mappen ikke finnes, vil programmet outputte "Mappen finnes ikke." Vi kan også bruke en try-catch blokk for å fange eventuelle feil som kan oppstå når vi prøver å sjekke om en mappe eksisterer.

Dypdykk:

Sjekk om en mappe eksisterer er en vanlig operasjon i de fleste programmeringsspråk. I C#, bruker vi Directory.Exists() metoden, men det finnes også andre måter å sjekke om en mappe eksisterer på. Vi kan for eksempel bruke DirectoryInfo klassen til å sjekke om en mappe eksisterer, eller vi kan bruke File.Exists() metoden for å sjekke om en fil finnes i en spesifikk mappe.

Se også:

Hvis du ønsker å lære mer om å arbeide med mapper i C#, kan du sjekke ut disse ressursene:

- Microsoft's offisielle dokumentasjon om Directory.Exists(): https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists
- Håndtering av mapper og filer i C#: https://www.c-sharpcorner.com/uploadfile/mahesh/filesysteminfo-class-in-C-Sharp/ 
- Dette YouTube videoen demonstrerer forskjellige måter å sjekke om en mappe eksisterer i C#: https://www.youtube.com/watch?v=X6erClYPBSI