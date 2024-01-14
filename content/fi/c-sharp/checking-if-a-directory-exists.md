---
title:    "C#: Tarkistetaan, onko hakemisto olemassa"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi sinun kannattaa tarkistaa, onko kansio olemassa C# -ohjelmoinnissa. Voit esimerkiksi haluta varmistaa, että tietty kansio on olemassa ennen kuin tallennat tiedostoja siihen tai haluat ehkä luoda uuden kansion, jos sitä ei vielä ole olemassa.

## Miten

Tämä toiminto on melko yksinkertainen C# -ohjelmoinnissa ja voidaan suorittaa käyttämällä Directory.Exists () -metodia.

```C#
using System;
using System.IO; //Sisällytä IO Namespacet

namespace TarkistaKansio
{
    class Program
    {
        static void Main(string[] args)
        {
            //Luodaan uusi kansiotie
            string kansiotie = @"C:\Kansio";

            //Tarkistetaan, onko kansio olemassa
            if (Directory.Exists(kansiotie))
            {
                // Tulostetaan viesti, jos kansio löytyy
                Console.WriteLine("Kansio on jo olemassa.");
            }
            else
            {
                // Luo uusi kansiotie
                Directory.CreateDirectory(kansiotie);
                // Tulostetaan viesti uuden kansion luomisesta
                Console.WriteLine("Uusi kansio luotu.");
            }
        }
    }
}
```

Tämän koodin tuloste riippuu siitä, onko kansio olemassa vai ei. Jos kansio on jo olemassa, tulosteena näkyy "Kansio on jo olemassa.", kun taas jos kansio luodaan, tulosteena näkyy "Uusi kansio luotu."

## Syventävä tarkastelu

Tarkistettaessa kansion olemassaoloa C# -ohjelmoinnissa on tärkeää muistaa, että Directory.Exists () -metodi ei tarkista kansiorakennetta. Jos siis tarkistat olemassaoloa esimerkiksi kansiotiellä "C:\Kansio\Alakansio", metodi palauttaa false, vaikka kansio "C:\Kansio" olisi olemassa.

Lisäksi on hyvä pitää mielessä, että Directory.Exists () -metodia voidaan käyttää tarkistamaan vain olemassaolevia kansioita, ei tiedostoja.

## Katso myös

- Microsoftin dokumentaatio Directory.Exists () -metodista: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netframework-4.8
- Infinitec:n opas tiedostojen ja kansioiden käsittelyyn C# -ohjelmoinnissa: https://www.infinitec.de/post/2011/03/29/File-System-Access-with-CSharp-Basics.aspx
- Suomenkielinen C# -ohjelmointisanasto: https://repl.it/talk/learn/C-OHJELMOINTISANASTO/37138