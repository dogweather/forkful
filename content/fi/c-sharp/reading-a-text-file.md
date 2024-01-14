---
title:                "C#: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen lukeminen on yksi perustaidoista C# -ohjelmoinnissa. Se on tärkeä taito, joka auttaa ohjelmoijia käsittelemään ja analysoimaan suuria määriä tietoa tiedostoista. Olipa kyseessä sitten tiedon tallentaminen, raporttien luominen tai tietojen visualisointi, tekstitiedostojen lukeminen on välttämätöntä lähes jokaisessa ohjelmassa.

## Miten tehdä

Tekstitiedoston lukeminen C# -kielellä on yllättävän yksinkertaista. Käytämme siihen StreamReader-luokkaa, joka tarjoaa monia käteviä metodeja tekstitiedoston lukemiseen. Katso seuraavia koodirivejä saadaksesi paremman käsityksen:

```C#
// Avaa tiedosto ja luo uusi StreamReader-olio
StreamReader reader = new StreamReader("tiedostonimi.txt");

// Lukee tiedoston sisällön kokonaisuudessaan ja tallentaa sen muuttujaan
string text = reader.ReadToEnd();

// Sulkee tiedoston lukemisen jälkeen
reader.Close();

// Tulostaa tiedoston sisällön konsoliin
Console.WriteLine(text);

```

Seuraava koodiesimerkki näyttää, kuinka voit lukea ja käsitellä tekstitiedostoa rivi kerrallaan. Tässä esimerkissä käytämme while-silmukkaa ja StreamReaderin ReadLine-metodia:

```C#
// Avaa tiedosto ja luo uusi StreamReader-olio
StreamReader reader = new StreamReader("tiedostonimi.txt");

// Luetaan tiedosto rivi kerrallaan ja tulostetaan jokainen rivi konsoliin
string line;
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}

// Sulkee tiedoston lukemisen jälkeen
reader.Close();

```

Näissä esimerkeissä käytämme File-luokkaa luomaan uuden tekstitiedoston ja kirjoittamaan siihen tietoa. Tämä on hyödyllistä esimerkiksi silloin, kun haluat tallentaa käyttäjältä saatuja tietoja tiedostoon:

```C#
// Luodaan uusi tekstitiedosto nimellä "uusi_tiedosto.txt"
File.Create("uusi_tiedosto.txt");

// Avataan tiedosto ja luodaan StreamWriter-olio
StreamWriter writer = new StreamWriter("uusi_tiedosto.txt");

// Kirjoitetaan tietoa tiedostoon
writer.WriteLine("Tervetuloa blogiimme!");
writer.WriteLine("Tutustu artikkeleihimme ja opi uutta C# -ohjelmoinnista.");

// Suljetaan tiedosto
writer.Close();

```

## Syventävä tieto

Tekstitiedostojen lukeminen on perustietoa C# -ohjelmoinnissa, mutta siitä löytyy myös monia edistyneempiä ominaisuuksia. Voit esimerkiksi määrittää erilaisia lukutapoja, kuten käsittelyä merkitsemättömien rivien kohdalla tai tiedoston koodauksen. Lisäksi voit käyttää muita luokkia, kuten FileStream tai BinaryReader, tekstitiedoston lukemiseen.

Jos haluat oppia lisää tekstitiedoston lukemisesta C# -kielellä, suosittelemme tarkistamaan C# -kielen dokumentoinnin aiheesta tai tutustumaan erilaisiin oppimateriaaleihin.

## Katso myös

- C# Language Reference: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/
- C# Programming Guide: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/
- Tutorialspoint: https://www.tutorialspoint.com/csharp/csharp_text_files.htm