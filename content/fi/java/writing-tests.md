---
title:                "Java: Testien kirjoittaminen"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmointiprojektit ovat suuria ja monimutkaisia, ja niissä voi tulla helposti virheitä. Testien kirjoittaminen auttaa varmistamaan, että koodi toimii odotetusti ja estää haitallisten virheiden pääsyä tuotantoympäristöön.

## Miten

### Asennus

Voit aloittaa testien kirjoittamisen käyttämällä testausta tukevaa sovellusta, kuten JUnitia. Asenna JUnit Mavenin kautta lisäämällä seuraava riippuvuus ```pom.xml```-tiedostoon:

```Java
<dependency>
  <groupId>org.junit.jupiter</groupId>
  <artifactId>junit-jupiter-api</artifactId>
  <version>5.7.2</version>
</dependency>
```

Voit myös asentaa sen IntelliJ:n avulla valitsemalla "File" ja sitten "Project Structure". Valitse "Modules" ja sitten "Dependencies". Napsauta "+"-painiketta ja valitse JUnit riippuvuus.

### Testiluokan luominen

Aloita kirjoittamalla uusi testiluokka, jossa voit kirjoittaa testejä koodiisi. Voit tehdä tämän luomalla uuden ```Test```-luokan, jonka nimi alkaa sanalla "Test". Esimerkiksi jos koodisi luokan nimi on "Calculator", niin testiluokan nimi olisi "CalculatorTest". Voit myös käyttää ```Crtl+Shift+T``` pikanäppäintä luodaksesi uuden testiluokan.

### Testimetodien luominen

Jokainen testiluokka voi sisältää useita testimetodeja, jotka testaavat eri osia koodistasi. Näiden metodien on noudatettava seuraavia sääntöjä:

- Metodin nimen alussa tulee olla sana "test", esim. testAdd()
- Metodin on palautettava ```void```
- Metodin ei tulisi hyväksyä parametreja
- Metodin tulisi aloittaa annotaatiolla ```@Test```

Tässä esimerkissä testataan laskimen ```add()```-metodia:

```Java
@Test
void testAdd() {
  Calculator calc = new Calculator();
  int result = calc.add(2, 3);
  assertEquals(5, result);
}
```

### Testien suorittaminen

Voit suorittaa testit napsauttamalla hiiren oikealla painikkeella testiluokkaa ja valitsemalla "Run `CalculatorTest`". Voit myös napsauttaa vihreää "Run"-näppäintä koodissa olevan nuolen vieressä. Näet sitten IntelliJ:ssä testien läpäisemisen tai mahdolliset virheilmoitukset.

## Syväsukellus

Testien kirjoittaminen auttaa sinua löytämään virheitä koodissasi aiemmin ja korjaamaan ne ennen kuin ne päätyvät tuotantoympäristöön. Se säästää aikaa ja minimoi koodinmuutosten riskin. Testojen avulla voit myös helposti havaita ja korjata mahdollisia sivuvaikutuksia, jotka voivat aiheuttaa hankalia virheitä tuotannossa.

On myös tärkeää muistaa, että testit eivät välttämättä kata kaikkia mahdollisia tapauksia, ja siksi on tärkeää jatkuvasti parantaa testikattavuutta. Tämän avulla voit varmistua siitä, että koodisi tekee oikean asian jokaisessa mahdollisessa tilanteessa.

## Katso myös
- [JUnit käyttöönotto](https://www.jetbrains.com/help/idea/junit.html#test-creation)
- [Testattavan koodin kirjoittaminen](https://devexp.io/writing-testable-code