---
title:    "C#: Tekstitiedoston lukeminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukeminen on keskeinen osa ohjelmoinnin oppimista. Tekstikäsikirjoitusten lukeminen on erityisen tärkeää, sillä se antaa meille arvokasta tietoa siitä, miten tiedostot tallennetaan, jäsennellään ja käsitellään tietokoneella. Tässä blogikirjoituksessa opimme, miten voimme lukea tekstikäsikirjoituksia käyttämällä C# -ohjelmointikieltä.

## Miten

Käytämme C# -ohjelmointikieltä lukemaan tiedostoja. Ensimmäisen vaiheen tulisi olla tiedoston avaaminen ja lukeminen. Se voidaan tehdä käyttämällä `StreamReader` -luokkaa, joka auttaa lukemaan tekstikäsikirjoituksia ja tallentamaan sen sisällön muuttujaan.

```C#
using System.IO;

// Avataan tiedosto "tiedostonimi.txt"
StreamReader tiedosto = new StreamReader("tiedostonimi.txt");

// Luetaan tiedoston sisältö ja tallennetaan se muuttujaan "sisalto"
string sisalto = tiedosto.ReadToEnd();
// Suljetaan tiedosto
tiedosto.Close();

// Tulostetaan tiedoston sisältö konsoliin
Console.WriteLine(sisalto);
```

Yllä olevassa esimerkissä luomme ensin uuden `StreamReader` -ilmentymän ja annamme sille tiedostonimen, jonka haluamme lukea. Tämän jälkeen käytämme `ReadToEnd()` -metodia lukemaan ja tallentamaan tiedoston sisällön `sisalto`-muuttujaan. Lopuksi suljemme tiedoston käyttämällä `Close()` -metodia.

### Tulos

Tulostamme konsoliin tiedoston sisällön käyttämällä `Console.WriteLine` -metodia ja antamalla sille `sisalto`-muuttujan.

```
Tervetuloa tekstikäsikirjoituksen maailmaan!
```

Voit myös lukea tekstikäsikirjoituksia rivin kerrallaan käyttämällä `ReadLine()` -metodia.

```C#
// Luetaan tiedoston ensimmäinen rivi ja tallennetaan se muuttujaan "rivi"
string rivi = tiedosto.ReadLine();
```

## Syvällinen sukellus

Lukeminen on vain yksi osa tiedoston käsittelyä. Voit myös muuttaa tiedoston sisältöä ja tallentaa sen takaisin käyttämällä `StreamWriter` -luokkaa.

```C#
// Luodaan uusi "StreamWriter" -ilmentymä ja annetaan sille tiedoston nimi
StreamWriter kirjoittaja = new StreamWriter("uusitiedosto.txt");

// Muutetaan tiedoston sisältöä ja tallennetaan se käyttämällä "WriteLine" -metodia
kirjoittaja.WriteLine("Tämä on uusi rivi.");
// Suljetaan tiedosto
kirjoittaja.Close();
```

Yllä olevassa esimerkissä käytämme `StreamWriter` -luokkaa tallentamaan uuden rivin tiedostoon. Voit myös käyttää muita metodeja, kuten `Write()` tai `Append()`, muokkaamaan tai lisäämään sisältöä tiedostoon.

## Katso myös

- [Microsoft C# -dokumentaatio](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [SoloLearn C# -kurssi](https://www.sololearn.com/Course/CSharp/)