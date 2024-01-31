---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:08.002051-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) betekent het versturen van je foutmeldingen, gescheiden van de normale uitvoer (stdout). Programmeurs doen dit om normale gegevens te scheiden van foutinformatie, wat helpt bij het loggen en debuggen.

## Hoe te:

In C#, schrijf naar stderr met behulp van `Console.Error.WriteLine()`. Het lijkt op `Console.WriteLine()`, alleen gericht op de foutstroom.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Bericht Standaarduitvoer."); // Gaat naar stdout
        Console.Error.WriteLine("Foutbericht!"); // Gaat naar stderr
    }
}
```

Voorbeelduitvoer als alles in orde is:

```
Bericht Standaarduitvoer.
```

Maar, als er iets mis is, zou je zien:

```
Bericht Standaarduitvoer.
Foutbericht!
```

Het foutbericht verschijnt in de console of kan worden omgeleid naar een bestand.

## Diepgaande Duik

Historisch gezien dateert het scheiden van stdout en stderr terug naar Unix-systemen, waar het schone gegevensverwerking en foutafhandeling mogelijk maakte. In C# (en .NET in het algemeen) vertegenwoordigt `Console.Out` stdout, terwijl `Console.Error` stderr vertegenwoordigt.

Je kunt beide omleiden met behulp van `Console.SetOut()` en `Console.SetError()`. Streams zoals `FileStream` of `StringWriter` kunnen de uitvoer vangen voor logging. Het is cruciaal in scenario's waar foutmeldingen niet mogen mengen met reguliere gegevens, zeg, wanneer stdout wordt gepijpt naar een ander programma.

## Zie Ook

- [Console.Error Eigenschap - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [.NET Stream Klasse - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.stream)
