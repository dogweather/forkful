---
date: 2024-01-20 17:53:58.254534-07:00
description: 'How to: (Kuinka?) .'
lastmod: '2024-03-13T22:44:56.591437-06:00'
model: gpt-4-1106-preview
summary: .
title: Tekstitiedoston lukeminen
weight: 22
---

## How to: (Kuinka?)
```C#
using System;
using System.IO;

class ReadTextFileExample
{
    static void Main()
    {
        string filePath = @"C:\example\tekstitiedosto.txt";

        if (File.Exists(filePath))
        {
            string content = File.ReadAllText(filePath);
            Console.WriteLine(content);
        }
        else
        {
            Console.WriteLine("Tiedostoa ei löydy.");
        }
    }
}
```
**Esimerkkituloste:**
```
Hei, tässä on esimerkkitiedoston tekstiä!
```

## Deep Dive (Syväluotaus)


### Historiallinen konteksti
Alkujaan tekstiedostojen lukeminen oli lähellä käyttöjärjestelmän toimintoja. C# tekee siitä helpompaa abstraktoimalla monimutkaiset asiat.

### Vaihtoehdot
Voit lukea tiedostoja rivittäin `File.ReadLines`-metodilla tai käyttää `StreamReader`-työkalua suurempiin tiedostoihin.

### Implementaation yksityiskohdat
`File.ReadAllText` lataa koko tiedoston muistiin, joten muista varovaisuus suurten tiedostojen kanssa. `StreamReader` lukee rivejä lennossa, joten muistin käyttö pysyy hallinnassa.

## See Also (Katso Myös)
- [Microsoft Docs: File.ReadAllText Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)
- [Microsoft Docs: StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [Microsoft Docs: File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
