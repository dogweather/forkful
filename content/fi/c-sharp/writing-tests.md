---
title:                "Ohjelmointitestien kirjoittaminen"
html_title:           "C#: Ohjelmointitestien kirjoittaminen"
simple_title:         "Ohjelmointitestien kirjoittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Testien kirjoittaminen on yksi tärkeimmistä osista ohjelmistonkehitysprosessia. Se on prosessi, jossa kirjoitetaan koodia ja suoritetaan se automaattisesti varmistaakseen, että ohjelmisto toimii odotetusti. Testien avulla voidaan havaita mahdollisia virheitä ja välittömästi korjata ne, mikä säästää aikaa ja vaivaa myöhemmin.

# Kuinka?

C# -ohjelmointikielessä testien kirjoittaminen on suhteellisen yksinkertaista. Alla on esimerkki yksikkötestin kirjoittamisesta ja sen tulostuksesta:

```
C# // Määritellään yksikkötesti luokka
public class CalculatorTests
{
    // Määritellään testi, joka tarkistaa yhteenlaskun toimivuuden
    [Test]
    public void TestAddition()
    {
        // Luodaan olio laskimelle
        Calculator calc = new Calculator();
        
        // Suoritetaan laskutoimitus ja tallennetaan tulos
        int result = calc.Add(2, 3);

        // Verrataan tulosta odotettuun arvoon
        Assert.AreEqual(5, result);
    }
}

```

Tämän testin ajamisen jälkeen näemme, että laskimen Add-metodi toimii odotetusti tulostaessaan 5 kahden luvun summana.

# Syvempi sukellus

Testien kirjoittamisella on juuret testauslajissa nimeltä testiautomaatio, joka alkoi kasvaa suosituksi 1980-luvulla. Nykyään testien kirjoittamisella on tärkeä rooli ketterässä ohjelmistokehityksessä ja se on olennainen osa jatkuvaa integrointia ja toimitusketjua. Lisäksi C# -ohjelmointikielessä on muitakin testauskehyksiä, kuten NUnit ja xUnit, jotka tarjoavat erilaisia ominaisuuksia ja toiminnallisuuksia testien kirjoittamiseen.

# Katso myös

Jos haluat lisätietoja testien kirjoittamisesta C# -ohjelmointikielessä, tutustu seuraaviin lähteisiin:

- [Microsoftin virallinen dokumentaatio C# -kielen testauksesta](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [NUnit -testauskehys](https://nunit.org/)
- [xUnit -testauskehys](https://xunit.net/)