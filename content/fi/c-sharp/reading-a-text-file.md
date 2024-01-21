---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:53:58.254534-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

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