---
title:                "C#: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on usein tarpeen ohjelmoinnissa, kun halutaan yhdistää erilaisia tekstejä tai merkkijonoja yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä esimerkiksi tulostettaessa tietoja käyttäjälle tai luotaessa tiedostoja. C#:n avulla tämä onnistuu helposti ja tehokkaasti.

## Kuinka tehdä

Stringien yhdistäminen C#:lla onnistuu käyttämällä yksinkertaisesti plus-merkkiä (+) yhdistämään halutut merkkijonot. Esimerkiksi seuraava koodi yhdistää kolme eri stringiä yhdeksi kokonaisuudeksi:

```C#
string nimi = "Matti";
string sukunimi = "Meikäläinen";
string kokoNimi = nimi + " " + sukunimi;
Console.WriteLine(kokoNimi);
```

Tämän koodin tuloste on "Matti Meikäläinen". Lisäksi C# tarjoaa myös String.Format-metodin, jonka avulla voidaan yhdistää merkkijonoja joustavammin ja esimerkiksi muotoilla ne halutulla tavalla. Esimerkiksi seuraava koodi tuottaa saman tuloksen kuin edellinen esimerkki, mutta käyttää String.Formatia:

```C#
string nimi = "Matti";
string sukunimi = "Meikäläinen";
string kokoNimi = string.Format("{0} {1}", nimi, sukunimi);
Console.WriteLine(kokoNimi);
```

Tämän koodin tuloste on myös "Matti Meikäläinen". On tärkeää huomata, että String.Formatilla voidaan myös muokata muotoilua ja lisätä esimerkiksi desimaalilukuja tai päivämääriä merkkijonoon.

## Syvällinen tarkastelu

C#:n toimintojen ansiosta stringien yhdistäminen on nopeaa ja helppoa. Kuitenkaan kaikki ei ole niin yksinkertaista kuin miltä se saattaa vaikuttaa. Esimerkiksi jokainen stringin yhdistämiseen käytetty + operaattori luo uuden merkkijonon. Tämä saattaa olla ongelmallista, jos yhdistetään suuri määrä merkkijonoja, sillä se voi aiheuttaa suorituskyvyn laskua. Tästä syystä on suositeltavaa käyttää String.Formatia, joka suorittaa yhdistämisen yhden ainoan merkkijonon avulla.

Lisäksi on hyvä tietää, että C#:ssä stringsit ovat muuttumattomia, eli ne eivät voi muuttua suorituksen aikana. Tämän vuoksi stringien yhdistäminen luo aina uuden merkkijonon, eikä muokkaa alkuperäistä stringiä.

## Katso myös

- [Stringien yhdistäminen C#:ssa](https://www.c-sharpcorner.com/article/concatenation-of-strings-in-c-sharp/)
- [String.Formatin käyttöohjeet](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netframework-4.8)
- [Tuple-luokka C#:ssa](https://docs.microsoft.com/en-us/dotnet/api/system.tuple?view=netframework-4.8)