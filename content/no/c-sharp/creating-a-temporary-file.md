---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å opprette en midlertidig fil? Ofte er det nyttig å ha midlertidige filer i en applikasjon for å lagre midlertidige data eller mellomresultater. Disse filene kan også brukes til å kommunisere mellom forskjellige deler av applikasjonen.

## Hvordan

For å opprette en midlertidig fil i C#, kan du bruke klassen `System.IO.Path` og `System.IO.File`. Det første du må gjøre er å bestemme hvor filen skal lagres. Dette kan være i brukerens midlertidige mappe ved å bruke `Path.GetTempPath()`.

```C#
string tempFilePath = Path.GetTempPath();
```

Deretter kan du bruke `Path.Combine()` for å kombinere denne stien med et unikt filnavn for å sikre at filen er unik.

```C#
string tempFileName = Path.GetRandomFileName();
string tempFilePath = Path.Combine(Path.GetTempPath(), tempFileName);
```

Nå kan du bruke `File.Create()` for å opprette og åpne filen for skriving.

```C#
using (FileStream fs = File.Create(tempFilePath)) 
{
    // Skrive til filen her
}
```

Etter at du er ferdig med å bruke filen, må du sørge for å stenge den ved hjelp av `fs.Close()` eller `fs.Dispose()`.

## Deep Dive

Når du bruker midlertidige filer, er det viktig å sørge for at du sletter dem når de ikke lenger er nødvendige. Dette kan gjøres ved å bruke `File.Delete()`-metoden i et `try-catch`-blokk for å håndtere eventuelle unntak.

```C#
try 
{
    File.Delete(tempFilePath);
}
catch (Exception e) 
{
    // Behandle eventuelle unntak her
}
```

Det er også verdt å merke seg at hvis du bruker midlertidige filer til å kommunisere mellom forskjellige deler av applikasjonen, bør du sørge for at filnavnene er unike for å unngå mulige kollisjoner. Dette kan gjøres ved hjelp av et system for å generere tilfeldige filnavn som `Path.GetRandomFileName()` som vist i eksempelet over.

## Se Også

- [MSDN: Opprette og bruke midlertidige filer på riktig måte i .NET] (https://docs.microsoft.com/nb-no/dotnet/standard/io/how-to-create-temporary-files)
- [C# Path-klasse] (https://docs.microsoft.com/nb-no/dotnet/api/system.io.path)
- [C# File-klasse] (https://docs.microsoft.com/nb-no/dotnet/api/system.io.file)