---
title:                "C#: Testien laatiminen"
simple_title:         "Testien laatiminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testaaminen on tärkeä osa ohjelmointia, sillä se auttaa varmistamaan koodin toimivuuden ja vähentämään mahdollisia virheitä. Testien avulla voit myös helposti havaita ja korjata bugeja ennen kuin ne aiheuttavat suurempia ongelmia.

## Kuinka

Testien kirjoittaminen C#:lla on helppoa ja ne voidaan lisätä helposti osaksi ohjelmakoodia. Alla on esimerkki yksinkertaisesta testitapauksesta, jossa testataan laskuoperaatioiden toimivuutta.

```C#
// luodaan testiluokka
public class LaskinTests
{
    // testifunktio, jossa suoritetaan laskinoperaatio ja verrataan tulosta odotettuun arvoon
    [Fact]
    public void TestaaSumma()
    {
        // asetetaan testattava luku
        int luku1 = 5;

        // asetetaan odotettu tulos
        int odotettuTulos = 10;

        // suoritetaan laskutoimitus
        int tulos = Laskin.Summa(luku1, luku2);

        // verrataan tulosta odotettuun arvoon
        Assert.Equal(odotettuTulos, tulos);
    }
}
```

Testien kirjoittamisessa hyödynnetään yleensä testing framework -kirjastoa, kuten esimerkiksi xUnit tai NUnit. Näiden avulla voit helposti suorittaa testit ja saada selkeän raportin niiden toimivuudesta.

## Syventävä sukellus

Testien kirjoittamisella on monia etuja, kuten koodin laadun parantaminen, virheiden havaitseminen ja ennaltaehkäisy sekä ohjelmiston luotettavuuden lisääminen. On tärkeää muistaa, että testien tulee olla kattavia ja peittää mahdollisimman monta erilaista skenaariota, jotta ne ovat mahdollisimman hyödyllisiä.

Toinen tärkeä huomioitava asia on testien jatkuvuus. Testien tulisi aina pysyä ajantasalla koodin muutosten kanssa, jotta ne ovat aina relevantteja ja luotettavia.

## Katso myös

- [xUnit](https://xunit.net/)
- [NUnit](https://nunit.org/)
- [Testausstrategiat ohjelmoinnissa](https://medium.com/@donireyes/testausstrategiat-ohjelmoinnissa-65018dd5a807)