---
title:                "Tiedoston lukeminen"
html_title:           "C#: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat tarvitsevat kyvyn lukea tekstitiedostoja tietojen käsittelyä ja tallentamista varten, joten tämä artikkeli auttaa heitä oppimaan, kuinka se tehdään käyttäen C# -ohjelmointikieltä.

## Kuinka

```C#
using System.IO;  // Tämä kirjasto tarvitaan tekstitiedoston lukemiseen ja kirjoittamiseen

// Avataan ja luetaan tekstitiedosto nimeltä "teksti.txt"
StreamReader lukija = new StreamReader("teksti.txt");

// Luetaan ensimmäinen rivi tekstitiedostosta
string rivi = lukija.ReadLine();

// Tulostetaan rivi konsoliin
Console.WriteLine(rivi);

// Suljetaan lukija
lukija.Close();
```

Output:

`Tämä on esimerkkiteksti, joka on tallennettu tekstitiedostoon.`

Tässä esimerkissä avataan ja luetaan tekstitiedosto nimeltä "teksti.txt". Lukemisen jälkeen ensimmäinen rivi tallennetaan muuttujaan ja tulostetaan konsoliin. Lopuksi lukija suljetaan, jotta muistia ei kuluteta turhaan.

## Syvällisempi sukellus

Jos haluat lukea tekstitiedoston sisällön kokonaisuudessaan ja käsitellä sitä jotenkin, voit käyttää `ReadToEnd()` -metodia sen sijaan, että lukisit tiedosto rivi kerrallaan. Tämä palauttaa koko tekstitiedoston sisällön yhtenä merkkijonona, jota voit sitten käsitellä haluamallasi tavalla.

```C#
// Luetaan koko tekstitiedoston sisältö ja tallennetaan muuttujaan
string sisalto = lukija.ReadToEnd();

// Tulostetaan koko sisältö konsoliin
Console.WriteLine(sisalto);

// Split-metodilla voidaan jakaa sisältö esimerkiksi rivinvaihdon perusteella
string[] rivit = sisalto.Split("\n");

// Tulostetaan rivien määrä
Console.WriteLine("Tiedostossa on " + rivit.Length + " riviä.");
```

Output:

`Tämä on esimerkkiteksti, joka on tallennettu tekstitiedostoon.
Tämä on seuraava rivi.
Ja tämä on vielä kolmas.`

`Tiedostossa on 3 riviä.`

Muista myös käyttää `try-catch` -lohkoa, jotta virhetilanteet voidaan hallita ja ohjelma ei kaadu, jos tiedostoa ei esimerkiksi löydy tai käyttäjällä ei ole lukuoikeutta.

## Katso myös

- [C# tiedoston lukeminen ja kirjoittaminen](https://www.tut.fi/pop/popSohv/HTML/textIO.html)
- [Tekstitiedostojen käsittely C# -ohjelmoinnissa](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
- [Tietojen tallentaminen ja lukeminen tekstitiedostoon C# -ohjelmoinnissa](https://www.c-sharpcorner.com/article/storing-data-in-a-text-file/)