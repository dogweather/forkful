---
title:    "C#: Tekstitiedoston kirjoittaminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on olennainen osa ohjelmointia, kun haluat tallentaa tietoa ja käsitellä sitä myöhemmin. Se on myös hyödyllistä, kun tarvitset tietojen siirtämistä eri ohjelmien välillä.

## Miten

Käyttämällä C#-ohjelmointikieltä, voit luoda uuden tekstitiedoston ja kirjoittaa siihen haluamasi sisällön seuraavalla koodilla:

```C#
// Luodaan uusi tekstitiedosto nimeltä "testi.txt"
var tekstitiedosto = new StreamWriter("testi.txt");

// Kirjoitetaan tiedostoon tekstiä
tekstitiedosto.WriteLine("Tervetuloa lukemaan tämä teksti!");
tekstitiedosto.WriteLine("Tämä on ensimmäinen rivi.");
tekstitiedosto.WriteLine("Ja tämä on toinen.");

// Suljetaan tiedosto
tekstitiedosto.Close();
```

Seuraava koodi lukee ja tulostaa tekstitiedoston sisällön konsoliin:

```C#
// Luetaan tiedosto ja tallennetaan sisältö muuttujaan
string sisalto = File.ReadAllText("testi.txt");

// Tulostetaan sisältö konsoliin
Console.WriteLine(sisalto);
```

Tämän koodin output olisi seuraavanlainen:

```
Tervetuloa lukemaan tämä teksti!
Tämä on ensimmäinen rivi.
Ja tämä on toinen.
```

## Syväsukellus

Voit myös käyttää erilaisia C#-luokkia ja metodeja kirjoittaaksesi tekstitiedoston. Esimerkiksi `StreamWriter`-luokka tarjoaa enemmän vaihtoehtoja kirjoittamiseen ja `File`-luokka tarjoaa monipuolisempia tapoja lukea tiedoston sisältöä. On tärkeää tutustua näihin luokkiin ja niiden tarjoamiin funktoihin, jotta voit löytää oikeat työkalut tarpeisiisi.

## Katso myös

- [C# StreamWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [C# File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file)