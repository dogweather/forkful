---
title:                "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor
Å opprette midlertidige filer er en vanlig oppgave i C# programmering. Det brukes vanligvis når man trenger å lagre midlertidig data eller når man ønsker å sikre at andre prosesser ikke endrer innholdet i en fil.

# Hvordan
```C#
// Opprett en midlertidig fil i temp-mappen
string tempFilePath = Path.GetTempFileName();
Console.WriteLine("Midlertidig fil opprettet: " + tempFilePath);
```

```C#
// Skriv data til den midlertidige filen
string tempFilePath = Path.GetTempFileName();
File.WriteAllText(tempFilePath, "Dette er en midlertidig fil.");
Console.WriteLine("Data skrevet til filen.");
```

```C#
// Sjekk om en midlertidig fil eksisterer
string tempFilePath = Path.GetTempFileName();
if (File.Exists(tempFilePath)) {
    Console.WriteLine("Midlertidig fil eksisterer.");
}
```

```C#
// Slett en midlertidig fil
string tempFilePath = Path.GetTempFileName();
File.Delete(tempFilePath);
Console.WriteLine("Midlertidig fil slettet.");
```

## Deep Dive
I C# programmering, kan man bruke klassen `Path` som inneholder metoder for å håndtere midlertidige filer. `GetTempFileName()` metoden oppretter en unik fil i temp-mappen og returnerer dens bane. Denne filen er garantert å være unik og blir automatisk slettet når programmet avsluttes. `WriteAllText()` metoden lar oss skrive data til den midlertidige filen, mens `File.Exists()` og `File.Delete()` metoder lar oss henholdsvis sjekke om en fil eksisterer og slette den.

# Se også
- [Microsoft Docs - Path Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-5.0)
- [Microsoft Docs - File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)
- [W3Schools - C# File Handling](https://www.w3schools.com/cs/csharp_files.asp)