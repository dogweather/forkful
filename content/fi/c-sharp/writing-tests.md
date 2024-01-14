---
title:    "C#: Testien kirjoittaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa testejä?

Testaus on erittäin tärkeä osa ohjelmistokehitystä, ja sen tavoitteena on varmistaa, että ohjelmisto toimii odotetulla tavalla. Testien kirjoittaminen auttaa löytämään mahdolliset virheet ja bugeja ennen kuin ohjelmisto julkaistaan loppukäyttäjille. Se myös auttaa varmistamaan, että uudet ominaisuudet eivät aiheuta ongelmia vanhoille toiminnoille. Jos olet ohjelmistokehittäjä, testien kirjoittaminen on ehdottomasti tärkeä taito hallita.

## Näin kirjoitat testejä C# -koodiin

```C#
using Microsoft.VisualStudio.TestTools.UnitTesting;

[TestClass]
public class CalculatorTests
{
    // Luodaan vaihtoehtoiset luvut testausta varten
    int numberOne = 5;
    int numberTwo = 10;

    [TestMethod]
    public void TestAddition()
    {
        // Luodaan oletettu vastaus summa-operaatiolle
        int expected = 15;
        // Kutsutaan metodia ja tallennetaan sen palauttama arvo
        int actual = Calculator.Add(numberOne, numberTwo);
        // Verrataan odotettua arvoa todelliseen
        Assert.AreEqual(expected, actual);
    }

    [TestMethod]
    public void TestSubtraction()
    {
        int expected = -5;
        int actual = Calculator.Subtract(numberTwo, numberOne);
        Assert.AreEqual(expected, actual);
    }
}

// Luodaan esimerkkilaskin luokka
public class Calculator
{
    public static int Add(int x, int y)
    {
        return x + y;
    }

    public static int Subtract(int x, int y)
    {
        return x - y;
    }
}
```

Testi-luokassa käytetään [TestClass] -attribuuttia ja [TestMethod] -attribuuttia testiluokkien ja -metodien määrittämiseen. Assert-luokan metodit, kuten AreEqual, ovat hyödyllisiä testien arviointiin ja virheiden löytämiseen.

## Syventävä sukellus testien kirjoittamiseen

Testien kirjoittaminen voi vaikuttaa vaikealta aluksi. On tärkeää varmistaa, että testit kattavat kaikki mahdolliset reitit koodissa, ja että ne ovat riittävän kattavia. Sinun pitäisi myös ajaa testisi useita kertoja ja varmistaa, että ne tuottavat saman tuloksen jokaisella ajon kerralla.

Kun olet kirjoittanut testit, käännytä projekti konsolikomennolla "dotnet test" tai Visual Studion Test Explorerin kautta. Jos kaikki testit menevät läpi ilman ongelmia, voit olla varma siitä, että koodisi toimii oikein.

## Katso myös

- [Microsoftin dokumentaatio testien kirjoittamiseen Visual Studiolla](https://docs.microsoft.com/en-us/visualstudio/test/writing-unit-tests-for-your-code?view=vs-2019)
- [Testauksen perusteiden opas C#:lla](https://www.tutorialspoint.com/csharp/csharp_unit_testing.htm)
- [Test-Driven Development (TDD) -konseptin ymmärtäminen](https://smartbear.com/learn/automated-testing/test-driven-development-guide/)