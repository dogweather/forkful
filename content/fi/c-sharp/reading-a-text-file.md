---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lukeminen tekstitiedosto tarkoittaa tekstin sisällön siirtämistä tiedostosta ohjelmaan. Ohjelmoijat tekevät tämän yleensä tietojen käsittelyä tai analysointia varten.

## Kuinka Tehdä:
Voit lukea tekstitiedoston C#-kielellä seuraavalla tavalla:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string text = System.IO.File.ReadAllText(@"C:\testi.txt");
        System.Console.WriteLine("Tiedoston sisältö: \n\n{0}", text);
    }
}
```

Edellisessä koodissa, `ReadAllText`-metodi lukee koko tiedoston sisällön kerralla. Suorittaessasi tätä, näet tulosteen, joka vastaa tiedostosi sisältöä.

## Syvä Sukellus:
Historiallisesti, tekstitiedostojen luku on ollut keskeinen osa ohjelmoinnin perusteita, sillä se mahdollistaa tiedon jakamisen ja tallentamisen. Mutta C# tarjoaa useita muitakin tapoja tekstin lukuun. Vaihtoehtoina ovat esimerkiksi `StreamReader` ja `File.ReadLines()`.

Ensimmäinen, eli `StreamReader`, on hyödyllinen, kun tiedostosi on erityisen suuri, sillä se lukee tiedon vähitellen eikä tarvitse ladata koko tiedostoa muistiin kerralla. Toinen tapa, `File.ReadLines()`, lukee tiedoston osissa ja on siksi tehokas myös suurissa tiedostoissa.

Haluat valita näistä menetelmistä sopivimman sovelluksesi tarpeiden mukaan, tarvittaessa myös yhdistellen niitä.

## Katso Myös:
Lisää tietoa ja ohjeita tekstitiedostojen lukemisesta C#-kielellä:

- Microsoftin ohjeet: https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file
- Stack Overflow keskustelut: https://stackoverflow.com/questions/4966476/c-sharp-read-file-line-by-line
- Tutorialspoint C# opas: https://www.tutorialspoint.com/csharp/csharp_file_io.htm