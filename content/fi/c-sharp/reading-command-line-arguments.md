---
title:    "C#: Komentoriviparametrien lukeminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Miksi lukea komentorivin argumentteja?

Komentorivin argumenttien lukeminen on tärkeä taito kaikille C#-ohjelmoijille. Se mahdollistaa ohjelman suorittamisen erilaisilla parametreilla, jolloin ohjelma voi toimia joustavammin ja tarjota käyttäjälle enemmän vaihtoehtoja. Lisäksi se tekee ohjelman ajoittamisesta helpompaa, koska eri skenaarioiden testaaminen ei vaadi koodimuutoksia.

### Kuinka lukea komentorivin argumentteja?

Komentorivin argumenttien lukeminen C#-kielellä on helppoa. Ensin luodaan Main-metodi, joka on ohjelman aloituspiste ja ottaa vastaan taulukon komentorivin argumenteista. Tämän jälkeen voidaan käyttää string-taulukon indeksöintiä, jotta halutut argumentit saadaan talteen. Tässä esimerkissä luetaan ja tulostetaan ensimmäinen ja toinen komentorivin argumentti:

```C#
static void Main(string[] args)
{
    string argument1 = args[0];
    string argument2 = args[1];
    Console.WriteLine(argument1);
    Console.WriteLine(argument2);
}
```

Ohjelma voi myös lukea useita argumentteja ja suorittaa erilaisia toimintoja riippuen annetuista parametreista. Esimerkiksi jos halutaan tarkistaa, onko komentorivillä annettu "-help" argumentti, voidaan käyttää seuraavaa koodia:

```C#
static void Main(string[] args)
{
    if (args.Contains("-help"))
    {
        Console.WriteLine("Ohjeet:");
        Console.WriteLine(" -help: Näyttää tämän viestin");
    }
}
```

Tässä tapauksessa ohjelma tulostaa ohjeet, mikäli komentorivillä on annettu "-help" argumentti. Muussa tapauksessa mitään ei tulosteta.

### Syvällinen tarkastelu komentorivin argumenttien lukemisesta

Kuten edellä mainituista esimerkeistä huomataan, komentorivin argumenttien lukeminen on yksinkertaista C#-kielellä. Argumentteja voidaan käyttää monipuolisesti ohjelman suorittamisessa ja vaihtoehtojen tarjoamisessa käyttäjälle. On myös mahdollista käsitellä useita argumentteja ja suorittaa tarkempia tarkistuksia, mikä tekee ohjelmasta joustavamman ja dynaamisemman.

### Katso myös

- [C#-voimakkuuden perusteet: Komentorivin argumentit](https://docs.microsoft.com/fi-fi/dotnet/csharp/fundamentals/program-structure/main-command-args)
- [Komentorivin argumenttien käsittely C#-kielellä](https://www.geeksforgeeks.org/command-line-arguments-in-c-sharp/) 
- [Miten käyttää komentorivin argumentteja C#-koodissa](https://www.c-sharpcorner.com/UploadFile/pranayamr/working-with-command-line-arguments-in-C-Sharp/)