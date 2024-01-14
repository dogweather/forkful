---
title:                "C#: Testien kirjoittaminen"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmointi on taito, jota voidaan jatkuvasti parantaa ja kehittää. Yksi tapa parantaa omaa kykyämme on kirjoittaa testejä. Testien kirjoittaminen auttaa varmistamaan, että koodimme toimii oikein ja vähentää mahdollisia bugien määrää. Se myös auttaa meitä ymmärtämään paremmin omaa koodiamme ja miten se toimii.

## Miten

Testaamisen avulla voimme varmistaa, että koodimme toimii oikein erilaisilla syötteillä ja eri tilanteissa. C# -kielellä testien kirjoittaminen on hyvin helppoa käyttämällä valmiita testikirjastoja, kuten NUnit tai xUnit. Alla on esimerkkikoodi, jossa testataan yksinkertaista metodia, joka laskee kahden numeron summan.

```C#
[Test]
public void SumTest()
{
    // Alustetaan testattava metodi
    Calculator calculator = new Calculator();

    // Testataan metodia annetulla syötteellä ja odotetulla tuloksella
    Assert.AreEqual(calculator.Sum(2, 3), 5);
}
```

Mikäli testit eivät mene läpi, saatamme huomata, että metodi ei toimi odotetulla tavalla. Voimme korjata koodia ja ajaa testit uudelleen, kunnes kaikki testit menevät läpi.

## Syväsukellus

Testien kirjoittaminen auttaa myös tekemään koodin parannuksia ja uusien ominaisuuksien lisäämistä helpommaksi ja turvallisemmaksi. Kun meillä on kattava testikokoelma, voimme olla varmoja siitä, että koodimme toimii joka kerta, kun teemme muutoksia.

Lisäksi testit voivat auttaa dokumentoimaan koodia. Kun kirjoitamme testejä, kirjoitamme käytännössä myös spesifikaation siitä, miten koodin tulisi toimia. Tämä tekee koodin ymmärtämisestä helpompaa ja nopeampaa myös muille tiimin jäsenille.

## Katso myös

- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [Test Driven Development (TDD)](https://fi.wikipedia.org/wiki/Test_Driven_Development)