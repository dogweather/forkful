---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tilapäisen tiedoston luominen on prosessi, jossa luodaan väliaikainen tiedosto ohjelman suorittamisen ajaksi. Tätä tehdään, koska se mahdollistaa suurien datamäärien turvallisen käsittelyn ja säilytyksen, jolloin muistin kuormitus pysyy minimissä.

## Näin teet:

Käytä C#:n `System.IO.Path`-luokkaa ja sen `GetTempFileName()`-metodia väliaikaisen tiedoston luomiseen kuten alla:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        // Luo väliaikainen tiedosto
        string tempFile = Path.GetTempFileName();

        // Tulosta tiedostonimi
        Console.WriteLine(tempFile);
    }
}
```

Suoritus tuottaa tulokseksi seuraavaa siis tiedostonimen:
```
C:\\Temp\\tmp4A8F.tmp
```

## Syvempi sukellus:

- Historia: Tilapäisten tiedostojen käyttö on ollut standardi käytäntö vuodesta 1972 alkaen, kun UNIX-käyttöjärjestelmä otti ne ensimmäisenä käyttöön.
- Vaihtoehdot: Voit itse hallinnoida väliaikaisia tiedostoja tiedostojärjestelmässä tai käyttää `MemoryStream`-objektia, jos tiedosto on riittävän pieni.
- Toteutusyksityiskohdat: `GetTempFileName()` luo 0 tavun tiedoston, joka on poistettu automaattisesti sovelluksen suorittamisen jälkeen.

## Katso myös:

- C# FileStream: https://docs.microsoft.com/fi-fi/dotnet/api/system.io.filestream?view=net-5.0
- Väliaikaisten tiedostojen hallinta: https://docs.microsoft.com/fi-fi/dotnet/standard/io/how-to-create-temporary-files