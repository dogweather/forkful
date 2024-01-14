---
title:    "C#: Sjekker om en mappe eksisterer"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som jobber med å håndtere filer og mapper, er det viktig å vite hvordan du kan sjekke om en mappe eksisterer. Dette kan være nyttig når du for eksempel ønsker å utføre en handling bare hvis mappen allerede finnes.

## Hvordan

Sjekke om en mappe eksisterer i C# er enkelt med hjelp av metoden `Directory.Exists()` fra `System.IO` biblioteket.

```C#
if (Directory.Exists(@"C:\Users\Bruker\Documents"))
{
    Console.WriteLine("Mappen finnes!");
}
else
{
    Console.WriteLine("Mappen finnes ikke :(");
}
```

Dette kodesnippet sjekker om mappen "Documents" under "Bruker" mappen eksisterer på stien `C:\Users\`. Hvis mappen finnes, vil programmet skrive ut "Mappen finnes!", ellers vil det skrive ut "Mappen finnes ikke :(".

## Dypdykk

For å forstå hvordan `Directory.Exists()` fungerer, er det nyttig å vite at den returnerer `true` hvis den gitte banen er en eksisterende mappe, og `false` hvis den ikke eksisterer eller det er en fil. Metoden tar også hensyn til unntak og tilgangstillatelser, slik at den vil returnere `false` hvis du ikke har tilgang til å sjekke om mappen eksisterer.

Man kan også bruke `DirectoryInfo` klassen for å sjekke om en mappe eksisterer. Denne klassen gir mer detaljert informasjon om en mappe og dens egenskaper.

```C#
DirectoryInfo dirInfo = new DirectoryInfo(@"C:\Users\Bruker\Desktop");

if (dirInfo.Exists)
{
    Console.WriteLine($"Mappen {dirInfo.Name} inneholder {dirInfo.GetFiles().Length} filer.");
}
else
{
    Console.WriteLine("Mappen eksisterer ikke :(");
}
```

Dette kodesnippet bruker `DirectoryInfo` klassen til å først opprette en instans av "Desktop" mappen under "Bruker" mappen og deretter sjekke om den eksisterer. Hvis mappen eksisterer, vil programmet skrive ut navnet på mappen og antall filer som den inneholder, ellers vil det skrive ut "Mappen eksisterer ikke :(".

## Se også

- [Microsoft dokumentasjon om Directory.Exists()](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [Microsoft dokumentasjon om DirectoryInfo](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo?view=netcore-3.1)