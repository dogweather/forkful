---
title:    "C#: Testien kirjoittaminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi kirjoittaa testejä? Testien kirjoittaminen voi tuntua turhalta lisätyöltä, mutta se voi todella säästää aikaa ja parantaa koodin laatua pitkällä tähtäimellä. Hyvin suunniteltujen ja toteutettujen testien avulla voit varmistaa, että koodisi toimii oikein ja vähentää virheiden esiintymistä tuotantoympäristössä.

## Kuinka
Koodin testaaminen C#-kielessä voi tuntua haastavalta, mutta se on erittäin tärkeä osa kehitysprosessia. Alla on muutamia esimerkkejä, jotka auttavat sinua aloittamaan.

```C#
// Luodaan luokka Calculator
public class Calculator
{
    // Metodi, joka laskee kahden luvun summan
    public int Sum(int a, int b)
    {
        return a + b;
    }
}

// Luodaan luokka testejä varten
// Tässä esimerkissä käytämme NUnit-testauskirjastoa
[TestFixture]
public class CalculatorTests
{
    // Luodaan testimetodi Sum-metodille
    [Test]
    public void SumTest()
    {
        // Luodaan instanssi Calculator-luokasta
        Calculator calculator = new Calculator();
        
        // Kutsutaan Sum-metodia ja varmistetaan, että saatu tulos on oikea
        Assert.AreEqual(4, calculator.Sum(2,2));
    }
}
```

Kun ajat testin, saat seuraavanlaisen tulosteen:

```bash
SumTest:
  Expected: 4
  But was:  5
```

Tässä tapauksessa testi epäonnistui, sillä summan pitäisi olla 4, mutta tulokseksi tuli 5. Tämä osoittaa, että jotain on vialla ja nyt voit korjata virheen ennen kuin se pääsee tuotantoympäristöön.

## Syvempi sukellus
Testien kirjoittaminen ei rajoitu vain yksikkötestaamiseen. On myös tärkeää testata integraatioita ja käyttöliittymää, jotta varmistetaan, että kaikki toimii yhteen halutulla tavalla. Testien kirjoittaminen auttaa myös dokumentoimaan koodin toimintaa ja parantamaan sen ylläpidettävyyttä.

## Katso myös
- [NUnit-testauskirjasto](https://nunit.org/)
- [Microsoftin ohjeet testien kirjoittamiseen C#-koodissa](https://docs.microsoft.com/en-us/dotnet/core/testing/?tabs=windows)