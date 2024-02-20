---
date: 2024-01-20 17:39:43.116341-07:00
description: "V\xE4liaikaistiedosto on v\xE4liaikaisessa k\xE4yt\xF6ss\xE4 oleva tiedosto,\
  \ joka katoaa, kun se ei ole en\xE4\xE4 tarpeen. Ohjelmoijat luovat niit\xE4 tallentamaan\
  \ dataa, joka\u2026"
lastmod: 2024-02-19 22:05:15.486199
model: gpt-4-1106-preview
summary: "V\xE4liaikaistiedosto on v\xE4liaikaisessa k\xE4yt\xF6ss\xE4 oleva tiedosto,\
  \ joka katoaa, kun se ei ole en\xE4\xE4 tarpeen. Ohjelmoijat luovat niit\xE4 tallentamaan\
  \ dataa, joka\u2026"
title: "V\xE4liaikaistiedoston luominen"
---

{{< edit_this_page >}}

## What & Why?
Väliaikaistiedosto on väliaikaisessa käytössä oleva tiedosto, joka katoaa, kun se ei ole enää tarpeen. Ohjelmoijat luovat niitä tallentamaan dataa, joka on tarpeellista vain hetkellisesti, esimerkiksi suorituksen aikana tai testattaessa.

## How to:
C# haihduttaa tiedostot sulavasti `Path`- ja `File`-luokkien avulla. Tsekkaa tämä:

```csharp
using System;
using System.IO;

class TemporaryFileExample
{
    static void Main()
    {
        // Luo väliaikaistiedosto
        string tempFileName = Path.GetTempFileName();

        // Kirjoita jotain tiedostoon
        File.WriteAllText(tempFileName, "Tämä on testi!");

        // Lue ja näytä sisältö
        string content = File.ReadAllText(tempFileName);
        Console.WriteLine(content);  // Outputti: Tämä on testi!

        // Siivoa ja poista väliaikaistiedosto
        File.Delete(tempFileName);
    }
}
```

Simple kuin sipuli. Tiedosto luodaan, siihen kirjoitetaan, sisältö näytetään, ja sitten tiedosto poistetaan.

## Deep Dive
Ennen vanhaan, levykkeiden ja rajoitetun tallennustilan aikakaudella, väliaikaistiedostot olivat kriittisiä. Nykyään, kun tallennustilaa on reilummin, ne ovat silti hyödyllisiä, etenkin järjestelmän roskien välttämiseksi ja tiedon käsittelyn optimoinnissa.

Vaihtoehtoja on: voit käyttää `TempFileCollectionia` tai luoda kustomoitun väliaikaistiedoston hallintaan. Tiedoston nimessä temp-alku voi olla hyvä perusta, mutta `Path.GetTempFileName()` antaa uniikin nimen, mikä vähentää yhteentörmäysten riskiä.

Järjestelmän väliaikaistiedostojen kansio on tyypillisesti paikka, minne väliaikaistiedosto kannattaa luoda, koska käyttöjärjestelmä ymmärtää pitää siivota siellä. `GetTempPath()` palauttaa tämän polun. 

## See Also
- [`Path.GetTempFileName`](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- [`Path.GetTempPath`](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettemppath)
- [`File.WriteAllText`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext)
- [`File.ReadAllText`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)
- [`File.Delete`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.delete)
