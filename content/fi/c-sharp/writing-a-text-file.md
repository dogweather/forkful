---
title:    "C#: Tiedoston kirjoittaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on tärkeä osa ohjelmointia useista syistä. Se voi olla hyödyllistä tallentaessaan tietoja ja aloittaessa ohjelman tietokantojen luomisen. Se myös helpottaa tiedon jakamista ja käyttöä muiden ohjelmien kanssa.

## Miten

Tekstitiedoston kirjoittaminen C#-kielellä on melko yksinkertaista. Se vaatii vain muutaman rivin koodia, mutta taito on erittäin hyödyllinen ohjelmointityössä. Alla on esimerkki siitä, miten voit luoda ja kirjoittaa tekstitiedoston käyttäen C#-kielen StreamWriter-luokkaa:

```C#
using (StreamWriter sw = new StreamWriter("tekstitiedosto.txt"))
{
    // Tähän voit kirjoittaa haluamasi tekstin
    sw.WriteLine("Tämä on esimerkki tekstistä.");
}
```
Koodi käyttää StreamWriter-luokkaa, joka mahdollistaa tiedostoon kirjoittamisen. Ensimmäiselle riville on lisätty "using", joka huolehtii resurssien vapauttamisesta, kun tiedostoa ei enää tarvita. Tämän jälkeen määritellään uusi StreamWriter-olio, jolle annetaan parametrina tiedoston nimi, johon halutaan kirjoittaa. Sen jälkeen voit käyttää `sw.WriteLine()` -metodia lisätäksesi haluamasi tekstin tiedostoon.

### Tulostettu sisältö

Kun ohjelma suoritetaan, se luo tekstitiedoston ja kirjoittaa siihen. Tarkista tiedoston sisältö avaamalla se tekstieditorilla tai lukemalla sisältö suoraan C#-koodista.

## Syvällisempi perehtyminen

Vaikka tekstitiedoston kirjoittaminen on suhteellisen helppoa C#-kielellä, siihen voi liittyä myös syvällisempiä käsitteitä, kuten tekstitiedostojen muotoilu ja käsitteleminen. On myös tärkeää huomata, että StreamWriter-luokkaan liittyy muutamia muita metodeja ja ominaisuuksia, joita voit tutkia lisää tarpeen mukaan.

## Katso myös

- [Tekstitiedostojen käsittely C#-ohjelmoinnissa](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time)
- [C# StreamWriter-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [C#-kielen perusteet](https://docs.microsoft.com/en-us/dotnet/csharp/tour-of-csharp)