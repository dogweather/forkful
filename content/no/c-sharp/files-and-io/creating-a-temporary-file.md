---
date: 2024-01-20 17:40:07.580006-07:00
description: "How to (Slik gj\xF8r du det) C# tilbyr flere m\xE5ter \xE5 opprette\
  \ midlertidige filer. Her er et enkelt eksempel med `Path.GetTempFileName()`."
lastmod: '2024-03-13T22:44:40.817410-06:00'
model: gpt-4-1106-preview
summary: "C# tilbyr flere m\xE5ter \xE5 opprette midlertidige filer."
title: Opprette en midlertidig fil
weight: 21
---

## How to (Slik gjør du det)
C# tilbyr flere måter å opprette midlertidige filer. Her er et enkelt eksempel med `Path.GetTempFileName()`:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Opprette en midlertidig fil
        string tempFilePath = Path.GetTempFileName();

        // Skrive no data til filen
        File.WriteAllText(tempFilePath, "Dette er en test!");

        // Viser stien til den midlertidige filen
        Console.WriteLine("Midlertidig fil opprettet ved: " + tempFilePath);

        // Rydder opp
        File.Delete(tempFilePath);
    }
}
```

Kjører du eksemplet, lager det en midlertidig fil, skriver tekst til den, og fjerner den til slutt. Kjøringen viser noe lignende dette:

```
Midlertidig fil opprettet ved: C:\Users\...\AppData\Local\Temp\tmp93D2.tmp
```

## Deep Dive (Dypdykk)
Historisk har midlertidige filer vært nyttige for å håndtere store datasett som ikke passer i minnet. I C# kan `GetTempFileName()` og `Path.GetRandomFileName()` hjelpe deg med å lage midlertidige filnavn, mens `TempFileCollection` i `System.CodeDom.Compiler` kan håndtere flere midlertidige filer sammenhengende.

Alternativt kan du bruke `FileStream` med `FileOptions.DeleteOnClose` for en midlertidig fil som automatisk slettes når strømmen lukkes, slik:

```C#
using (FileStream temporaryFileStream = new FileStream(Path.GetTempFileName(), FileMode.Create, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.DeleteOnClose))
{
    // Bruk temporaryFileStream her
}
// Filen er slettet her
```

Implementeringsdetaljer inkluderer valg av katalog for midlertidige filer (`Path.GetTempPath()`) og sikkerhetshensyn rundt tilgangskontroll for filene du oppretter.

## See Also (Se også)
- Microsoft Dokumentasjon for `GetTempFileName()`: [Official Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- Artikler om `FileOptions.DeleteOnClose`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.fileoptions?view=netcore-3.1#fields)
