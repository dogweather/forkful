---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "C#: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Kuten kaikissa tietokoneohjelmissa, satunnaisten numeroiden generointia tarvitaan myös C# -ohjelmoinnissa. Satunnaisilla numeroilla voi esimerkiksi simuloida pelattavan pelin eri vaihtoehtoja tai luoda salasanan, joka on mahdollisimman vaikea arvata. 

## How To

Satunnaisia numeroita voidaan generoida C#:ssa rand() -funktion avulla. Tämä funktio palauttaa liukuluvun välillä 0.0 ja 1.0. Alla on esimerkki, miten sitä voidaan hyödyntää: 

```C#
using System;

class MainClass {
  public static void Main (string[] args) {
    Console.WriteLine("Satunnaisesti generoitu kokonaisluku: " + rand());
  }
}
```

Syötteellä:

```
Satunnaisesti generoitu kokonaisluku: 0.375514
```

Voimme myös määrittää rand() -funktiolle ala- ja ylärajan, jolloin generoidut numerot ovat tällä välillä. Esimerkiksi seuraava koodi generoi satunnaisen kokonaisluvun väliltä 1-10:

```C#
using System;

class MainClass {
  public static void Main (string[] args) {
    Random rnd = new Random();
    int satunnainenNumero = rnd.Next(1,11);
    Console.WriteLine("Satunnaisesti generoitu kokonaisluku väliltä 1-10: " + satunnainenNumero);
  }
}
```

Syötteellä:

```
Satunnaisesti generoitu kokonaisluku väliltä 1-10: 7
```

## Deep Dive 

Rand() -funktio käyttää C:n rand() -funktiota, joka perustuu lineaariseen kongruenssimenetelmään. Tämä tarkoittaa, että jokainen generoitu luku riippuu edellisestä generoidusta luvusta ja funktiolla on myös periodisuus eli se palauttaa samat numerot tietyllä toistuvalla syötteellä. Sen vuoksi oikeaoppisessa käytössä suositellaan alkuarvon asettamista seed-toiminnolla ennen rand()-funktion käyttöä. 

## See Also

- [C#:n dokumentaatio satunnaisista numeroista](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Miksi C# on suosittu kieli ohjelmoinnissa?](https://medium.com/@aeriontech/why-c-has-become-a-popular-language-for-programmers-6546b33ddbfe)
- [Miten satunnaisia numeroita voidaan käyttää pelinkehityksessä?](https://www.gamasutra.com/blogs/LeviDmith/20170714/300866/Random_Numbers_and_Game_Design.php)