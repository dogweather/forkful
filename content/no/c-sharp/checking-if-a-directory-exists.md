---
title:                "C#: Kontrollere om en mappe eksisterer"
simple_title:         "Kontrollere om en mappe eksisterer"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å vite om en mappe eksisterer før du prøver å aksessere eller manipulere den. Dette sikrer at programmet ditt fungerer korrekt og ikke krasjer hvis mappen ikke eksisterer.

## Hvordan

For å sjekke om en mappe eksisterer i C #, bruker du `Directory.Exists` metoden. Denne metoden tar inn en streng som representerer banen til mappen du vil sjekke, og returnerer en boolsk verdi som indikerer om mappen eksisterer eller ikke.

```C#
if (Directory.Exists("C:\\Users\\Desktop\\MyFolder")) {
    Console.WriteLine("Mappen eksisterer.");
} else {
    Console.WriteLine("Mappen eksisterer ikke.");
}
```

Output:
```
Mappen eksisterer.
```

Hvis mappen ikke eksisterer, kan du også opprette den ved hjelp av `Directory.CreateDirectory` metoden.

```C#
if (!Directory.Exists("C:\\Users\\Desktop\\MyFolder")) {
    Directory.CreateDirectory("C:\\Users\\Desktop\\MyFolder");
    Console.WriteLine("Mappen er opprettet.");
} else {
    Console.WriteLine("Mappen eksisterer allerede.");
}
```

Output:
```
Mappen er opprettet.
```

## Dypdykk

I tillegg til å sjekke om en mappe eksisterer, kan du også sjekke om en fil eksisterer på samme måte ved å bruke `File.Exists` metoden. Begge metodene bruker Windows API for å sjekke filsystemet, så de vil fungere riktig på alle operativsystemer som støttes av C #.

Det er også viktig å merke seg at selv om en mappe eller fil eksisterer når du sjekker, kan den fortsatt bli slettet eller flyttet av brukeren mens programmet ditt kjører. Så sørg for å håndtere eventuelle feil som kan oppstå i disse situasjonene.

## Se Også
- [Directory.Exists metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [File.Exists metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.exists)
- [Håndtering av feil i C#](https://www.tutorialspoint.com/csharp/csharp_exceptions.htm)