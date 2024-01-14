---
title:                "C#: Merkkijonon pituuden löytäminen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Yksi yleisesti käytetyistä ohjelmoinnin tehtävistä on merkkijonon pituuden laskeminen. Tämä tehtävä on tärkeä monissa sovelluksissa, kuten tekstinkäsittelyohjelmissa, tietokannoissa ja verkkosivustoilla. Siksi on tärkeää ymmärtää, miten tämä tehtävä suoritetaan ja mitä vaihtoehtoja on käytettävissä.

## Miten tehdä

C#-kielen avulla voimme helposti saada merkkijonon pituuden laskemiseen tarvittavan tiedon. Tässä on esimerkki koodista, joka tulostaa käyttäjän antaman merkkijonon pituuden:

```C#
//Kysytään käyttäjältä syötettä
Console.WriteLine("Kirjoita jotain:");
string input = Console.ReadLine();

//Lasketaan merkkijonon pituus ja tulostetaan se
int length = input.Length;
Console.WriteLine("Merkkijonon pituus on " + length);
```

Koodin suorittaminen antaa seuraavan tulosteen, jos käyttäjä antaa syötteeksi "Tämä on esimerkki":

```
Merkkijonon pituus on 18
```

On myös useita muita tapoja saada merkkijonon pituus C#-kielellä. Voit esimerkiksi käyttää `String.Length` -ominaisuutta, joka palauttaa saman informaation. Tärkeintä on kuitenkin muistaa, että merkkijonojen pituus lasketaan kirjainten määrällä, mukaan lukien välilyönnit.

## Syvällisempi tarkastelu

Merkkijonon pituus lasketaan C#-kielellä käyttäen `Length`-ominaisuutta, joka sisältyy `String`-luokkaan. Tämä ominaisuus palauttaa tiedon merkkien määrästä kyseisessä merkkijonossa. On hyvä muistaa, että merkkijonon pituus ei ole vakio, vaan se voi muuttua sen mukaan, kuinka paljon käyttäjä syöttää dataa. Myös `string`-luokka tarjoaa erilaisia metodeja ja ominaisuuksia merkkijonojen manipuloimiseksi, kuten `ToUpper()` ja `ToLower()`.

## Katso myös

- [Microsoftin dokumentaatio merkkijonon pituuden laskemisesta käyttäen C#-kieltä (englanniksi)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/types/how-to-determine-the-length-of-a-string)
- [Codecademyn opetusmateriaali merkkijonojen käsittelystä C#-kielellä (englanniksi)](https://www.codecademy.com/learn/learn-c-sharp/modules/learn-csharp-methods/cheatsheet)