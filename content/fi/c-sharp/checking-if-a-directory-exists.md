---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- fi/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:17.037311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Hakemiston olemassaolon tarkistaminen C#:ssa tarkoittaa kansion läsnäolon varmistamista määritetyssä polussa tiedostojärjestelmässä. Ohjelmoijat tekevät tämän välttääkseen virheitä, kuten yrittämistä lukea tai kirjoittaa olemattomaan hakemistoon, varmistaen sujuvamman tiedosto- ja hakemistokäsittelyn.

## Kuinka:

### Käyttäen System.IO

C# tarjoaa `System.IO` nimiavaruuden, joka sisältää `Directory` luokan, tarjoten suoran tavan tarkistaa hakemiston olemassaolo `Exists` metodin avulla.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Tarkista, onko hakemisto olemassa
        bool directoryExists = Directory.Exists(directoryPath);

        // Tulosta tulos
        Console.WriteLine("Hakemisto on olemassa: " + directoryExists);
    }
}
```

**Esimerkkituloste:**

```
Hakemisto on olemassa: False
```

Jos hakemisto todellakin on olemassa polussa `C:\ExampleDirectory`, tuloste on `True`.

### Käyttäen System.IO.Abstractions yksikkötestaukseen

Kun kyse on koodisi yksikkötestattavuuden mahdollistamisesta, erityisesti kun se vuorovaikuttaa tiedostojärjestelmän kanssa, `System.IO.Abstractions` paketti on suosittu valinta. Se mahdollistaa tiedostojärjestelmän toimintojen abstrahoinnin ja mockaamisen testeissäsi. Tässä on miten voit tarkistaa hakemiston olemassaolon käyttäen tätä lähestymistapaa:

Ensin, varmista että olet asentanut paketin:

```
Install-Package System.IO.Abstractions
```

Sitten, voit injektoida `IFileSystem`-rajapinnan luokkaasi ja käyttää sitä tarkistamaan, onko hakemisto olemassa, mikä mahdollistaa helpomman yksikkötestauksen.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Hakemisto on olemassa: " + directoryExists);
    }
}
```

**Esimerkkituloste:**

```
Hakemisto on olemassa: False
```

Tämä lähestymistapa erottaa sovelluslogiikkasi suorasta pääsystä tiedostojärjestelmään, tehden koodistasi modulaarisemman, testattavamman ja ylläpidettävämmän.
