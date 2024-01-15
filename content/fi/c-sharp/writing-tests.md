---
title:                "Testien kirjoittaminen"
html_title:           "C#: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla testit varmistat, että koodisi toimii odotetulla tavalla ja että muutokset eivät riko jo olemassa olevaa toiminnallisuutta.

## Kuinka tehdä

Testien kirjoittaminen C#:ssa on helppoa. Käytämme siihen valmiita testikehyksiä, kuten *NUnit* tai *xUnit*, jotka tekevät testien kirjoittamisesta selkeää ja yksinkertaista.

Seuraavassa esimerkissä luomme yksinkertaisen testin, joka varmistaa, että laskinluokka suorittaa yhteen- ja vähennyslaskut oikein:

```C#
public class Calculator
{
    public int Add(int x, int y)
    {
        return x + y;
    }

    public int Subtract(int x, int y)
    {
        return x - y;
    }
}

[TestFixture] // NUnit-kirjastosta
public class CalculatorTests 
{
    [Test] // Merkintä kertoo, että tämä metodi on testi
    public void Add_AddsTwoValues_CorrectResult()
    {
        // Valmistele: luodaan laskinolio, jonka haluamme testata
        var calculator = new Calculator();

        // Toimi: suoritetaan laskutoimitus
        var result = calculator.Add(2, 2);

        // Varmista: tarkastetaan, että saatu tulos on odotettu
        Assert.AreEqual(4, result); // Testi epäonnistuu, jos arvot eivät ole samat
    }

    [Test]
    public void Subtract_SubtractsTwoValues_CorrectResult()
    {
        var calculator = new Calculator();
        var result = calculator.Subtract(10, 5);
        Assert.AreEqual(5, result);
    }
}
```

Kun ajamme testit, saamme seuraavan tulosteen:

```
Tests run: 2, Passed: 2, Failures: 0, Inconclusive: 0, Skipped: 0
```

Mikäli tulokset eivät vastaa odotettua, testi epäonnistuu ja näemme tarkemman virheilmoituksen. Näin voimme nopeasti havaita mahdolliset ongelmat ja korjata ne ennen kuin ne päätyvät tuotantokoodiin.

## Syvällisempi sukellus

Testien kirjoittaminen on myös tärkeää koodin laadun parantamiseksi. Hyvin kirjoitetut testit toimivat myös dokumentaationa koodin toiminnallisuudesta ja auttavat uusia kehittäjiä ymmärtämään koodia.

C#:ssa testien kirjoittaminen on helppoa, kun käytämme valmiita testikehyksiä. Voimme myös hyödyntää *Mock* kirjastoa luodessamme ohjelmalle riippuvuuksia, jotta voimme simuloida erilaisia testitilanteita.

On myös tärkeää muistaa, että testien kirjoittaminen ei korvaa manuaalista testausta, vaan täydentää ja nopeuttaa sitä.

## Katso myös

[.NET Testauskirjasto: NUnit] (https://nunit.org/)

[xUnit.net Test Framework] (https://xunit.net/)

[C# Tester] (https://www.c-sharpcorner.com/uploadfile/rahul4_saxena/teststestscsharpworksheet11262005022424am/teststestscsharpworksheet.aspx/)