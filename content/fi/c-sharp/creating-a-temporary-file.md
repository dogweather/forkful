---
title:    "C#: Väliaikaisen tiedoston luominen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda tilapäistiedosto?

Tilapäistiedostojen luominen on yleinen käytäntö C# -ohjelmoinnissa. Niitä käytetään usein tiedon tallentamiseen väliaikaisesti, esimerkiksi ohjelman suorituksen aikana. Tilapäistiedostoja voidaan myös käyttää tilapäisten prosessien väliaikaiseen tallentamiseen, jotka eivät tarvitse pysyvää tallennuspaikkaa.

## Kuinka tehdä se?

Tilapäistiedostojen luominen C# -ohjelmoinnissa on helppoa. Voit käyttää "Path.GetTempFileName()" -funktiota luodaksesi tilapäistiedoston ja tallentaa sen muuttujaan. Tämän jälkeen voit käyttää muuttujaa kirjoittaaksesi tai lukeaksesi tiedostoon tarvittavat tiedot.

```C#
string tempFile = Path.GetTempFileName();
Console.WriteLine(tempFile);
```

Tämä koodi luo tilapäistiedoston ja tulostaa sen polun konsoliin. Voit myös käyttää "File.WriteAllText()" ja "File.ReadAllText()" -funktioita kirjoittaaksesi ja lukemalla tiedostoon.

```C#
File.WriteAllText(tempFile, "Tämä on tilapäistiedostoon tallennettava teksti.");
string text = File.ReadAllText(tempFile);
Console.WriteLine(text);
```

Huomaa, että nämä esimerkit ovat vain yksinkertaisia tapoja käyttää tilapäistiedostoja. C# tarjoaa monia muitakin tapoja luoda, kirjoittaa ja lukea niitä.

## Syvempi sukellus

Tilapäistiedostojen luomisella on myös muita etuja. Yksi niistä on se, että ne poistetaan automaattisesti, kun ohjelma sulkeutuu. Tämä säästää tilaa ja estää turhien tiedostojen kertymisen järjestelmään.

Lisäksi tilapäistiedostot luodaan automaattisesti ainutkertaisella nimellä, joten ongelmat nimien kaksoiskäytön kanssa eivät ole mahdollisia. Tämä tekee tilapäistiedostoista turvallisempia käyttää kuin manuaalisesti luodut tiedostot.

## Katso myös

- [C# -opas](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [Tilapäisten tiedostojen opas](https://docs.microsoft.com/en-us/dotnet/standard/io/temporary-files)
- [Path-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-5.0)