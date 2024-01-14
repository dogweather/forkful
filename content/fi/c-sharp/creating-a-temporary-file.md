---
title:    "C#: Väliaikaisen tiedoston luominen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi luoda tilapäistiedosto?

Tilapäistiedoston luominen on hyödyllinen ohjelmointitaito, jota voidaan käyttää monissa erilaisissa tilanteissa. Se tarjoaa mahdollisuuden tallentaa väliaikaisia tietoja, joita ei tarvita pitkäaikaiseen tallennukseen tai joita ei haluta pitää pysyvästi järjestelmässä. Tässä blogikirjoituksessa käsitellään tarkemmin, miksi haluat ehkä luoda tilapäistiedoston ja miten se voidaan tehdä C# -ohjelmointikielellä.

## Miten luoda tilapäistiedosto C# -kielellä

Tilapäistiedoston luominen C# -ohjelmointikielellä on melko yksinkertaista. Se voidaan tehdä käyttämällä .NET Frameworkin luokkaa “Path” ja sen metodia “GetTempFileName()”.

```C#
string tempFile = Path.GetTempFileName();
```

Yllä oleva koodinpätkä luo tilapäistiedoston ja tallentaa sen polun ja nimen “tempFile” -muuttujaan. Voit sitten käyttää tätä muuttujaa käsitelläksesi tilapäistiedostoa haluamallasi tavalla.

```C#
Console.WriteLine("Tilapäistiedoston polku ja nimi: " + tempFile);

//output
//Tilapäistiedoston polku ja nimi: C:\Users\Käyttäjä\AppData\Local\Temp\tmp5DAF.tmp
```

Yllä oleva koodinpätkä tulostaa tilapäistiedoston polun ja nimen konsoliin. Huomaathan, että tilapäistiedosto tallentuu oletusarvoisesti käyttäjän “Temp” -kansioon.

## Syvempi sukellus tilapäistiedoston luomiseen

Vaikka tilapäistiedoston luominen onkin näin yksinkertaista C# -ohjelmointikielellä, on hyvä ymmärtää paremmin miksi ja kuinka se toimii. Tilapäistiedosto luodaan oletusarvoisesti käyttäjän “Temp” -kansioon, mutta sen sijaintia voidaan myös muuttaa tarvittaessa. Tiedoston tyyppi on myös “.tmp”, joka voi olla hyödyllinen tunnistaessaan sitä ohjelmassa.

On myös tärkeää huomata, että tilapäistiedosto poistetaan automaattisesti, kun ohjelma suljetaan. Tämä tarkoittaa sitä, että et joudu huolehtimaan tilapäistiedostojen siivoamisesta manuaalisesti.

## Lisätietoja tilapäistiedostoista

Jos haluat tutustua tarkemmin tilapäistiedostojen luomiseen ja niiden käyttöön C# -ohjelmointikielessä, voit lukea lisää seuraavista lähteistä:

- [Microsoft Docs - Tilapäisten tiedostojen luominen](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8)
- [C# Ylilauta - Tilapäisten tiedostojen luominen](https://ylilauta.org/c/16/t526940)

## Katso myös

- [Microsoft Docs - Path-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netframework-4.8)
- [GitHub - C# Tilapäistiedoston luominen](https://github.com/csharpprogramming/Csharp-Tilapäistiedoston-luominen)

Tilapäistiedoston luominen C# -ohjelmointikielellä on hyödyllinen taito, jota kannattaa opetella ja ottaa käyttöön omassa koodauksessa. Toivottavasti tämä blogikir