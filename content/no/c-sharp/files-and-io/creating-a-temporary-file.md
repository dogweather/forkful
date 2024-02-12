---
title:                "Opprette en midlertidig fil"
aliases:
- /no/c-sharp/creating-a-temporary-file/
date:                  2024-01-20T17:40:07.580006-07:00
model:                 gpt-4-1106-preview
simple_title:         "Opprette en midlertidig fil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Midlertidige filer er kortlivet dataoppbevaring på disk. Vi lager dem for sikker og flyktig datahåndtering, ofte ved håndtering av store datamengder eller for å redusere minnebruk.

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
