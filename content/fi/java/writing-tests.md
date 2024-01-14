---
title:                "Java: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa Java-ohjelmointia, sillä se auttaa varmistamaan, että koodi toimii oikein ja se on helposti ylläpidettävissä. Lisäksi testien avulla pystytään havaitsemaan mahdolliset virheet ja puutteet koodissa ennen sen julkaisemista.

## Kuinka

Testien kirjoittaminen Java-ohjelmointikielellä on helppoa ja vaatii vain muutamia perusaskelia. Ensinnäkin, sinun tarvitsee sisällyttää JUnit-testaustyökalu projektisi riippuvuuksiin. Tämän jälkeen voit käyttää JUnitin @Test -annotaatioita testien luomiseen. 

```Java
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class CalculatorTest {

	@Test
	public void testAddition() {
		Calculator calc = new Calculator();
		int result = calc.add(2, 3);
		assertEquals(5, result);
	}
}
```

Yllä olevassa esimerkissä luomme yksinkertaisen testin, joka testaa laskimen lisäysmetodia. @Test -annotaatio kertoo JUnitille, että tämä metodi on testi, jonka tulee suorittaa tarkistettava laskenta ja Assert-metodi vertaa odotettua ja palautettua tulosta. 

## Deep Dive

Testien kirjoittaminen on tehokas tapa varmistaa, että koodisi toimii halutulla tavalla. Lisäksi se auttaa myös parantamaan koodin laatua ja ylläpidettävyyttä. Hyvät testit kattavat kaikki mahdolliset rajatapaukset ja huomioivat myös virheiden käsittelyn.

JUnit ei ole ainoa Java-ohjelmointikielen testaustyökalu, vaan on olemassa myös muita vaihtoehtoja, kuten TestNG ja Mockito. Näitä työkaluja voi käyttää yhdessä JUnitin kanssa saadaksesi monipuolisemmat testit ja testaustaustan.

On myös tärkeää muistaa, että testien kirjoittaminen ei ole vain kertaluontoinen tapahtuma, vaan niitä tulee päivittää ja ylläpitää koodin muuttuessa ja kehittyessä.

## Katso myös

- [JUnit Documentation](https://junit.org/junit4/)
- [TestNG Documentation](https://testng.org/doc/)
- [Mockito Documentation](https://site.mockito.org/)