---
date: 2024-01-20 17:53:58.254534-07:00
description: "Lukemalla tekstitiedoston C#:ssa voit hakea dataa tiedostoista ohjelmasi\
  \ k\xE4ytt\xF6\xF6n. T\xE4m\xE4 on v\xE4ltt\xE4m\xE4t\xF6n taito tiedon k\xE4sittely\xE4\
  , s\xE4ilytyst\xE4 ja analysointia\u2026"
lastmod: '2024-03-13T22:44:56.591437-06:00'
model: gpt-4-1106-preview
summary: "Lukemalla tekstitiedoston C#:ssa voit hakea dataa tiedostoista ohjelmasi\
  \ k\xE4ytt\xF6\xF6n."
title: Tekstitiedoston lukeminen
weight: 22
---

## What & Why? (Mitä ja miksi?)
Lukemalla tekstitiedoston C#:ssa voit hakea dataa tiedostoista ohjelmasi käyttöön. Tämä on välttämätön taito tiedon käsittelyä, säilytystä ja analysointia varten.

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
