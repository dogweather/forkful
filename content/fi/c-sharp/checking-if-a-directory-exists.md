---
title:                "Tarkistetaan, onko hakemisto olemassa"
date:                  2024-01-19
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Tarkistetaan, onko hakemisto olemassa, jotta välttämme virheitä yrittäessämme käsitellä sitä. Koodarit tekevät tämän varmistaakseen, ettei sovellus kaadu eikä tieto katoa.

## How to: (Kuinka tehdä:)
Käytä `Directory.Exists` -metodia. Se palauttaa `true`, jos hakemisto on olemassa, ja `false`, jos ei. Esimerkki: 

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\test";
        
        if (Directory.Exists(path))
        {
            Console.WriteLine($"{path} exists.");
        }
        else
        {
            Console.WriteLine($"{path} does not exist.");
        }
    }
}
```

Sample Output:

```
C:\test exists.
```
tai jos hakemistoa ei ole,
```
C:\test does not exist.
```

## Deep Dive (Syväsukellus)
Hakemiston olemassaolon tarkistus on ollut oleellinen osa ohjelmistokehitystä jo varhaisista käyttöjärjestelmistä lähtien. Tiedostojärjestelmien hallinta on keskeistä, ja ennen `System.IO`-nimiavaruuden myötä tuotuja nykyaikaisia apuvälineitä kehittäjien tuli turvautua alhaisemman tason kutsuihin. Vaihtoehtoisia tapoja tarkistaa hakemiston olemassaolo ovat esimerkiksi `FileInfo`- tai `DriveInfo`-olioiden käyttö, mutta `Directory.Exists` on yksinkertaisin ja suoraviivaisin lähestymistapa. Sen toteutus hyödyntää alustan riippumattomuutta, mikä tarkoittaa sitä, että sovellukset toimivat saumattomasti eri käyttöjärjestelmien välillä.

## See Also (Katso Myös)
- Microsoft Docs `Directory.Exists` method: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists
- MSDN File and Stream I/O: https://docs.microsoft.com/en-us/dotnet/standard/io/
- MSDN How to: Enumerate Directories and Files: https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-enumerate-directories-and-files
