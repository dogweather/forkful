---
title:                "Testien kirjoittaminen"
html_title:           "Java: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi
Testien kirjoittaminen on tärkeä ja olennainen osa Java-ohjelmointia, koska se auttaa varmistamaan koodin toimivuuden ja parantaa sen luotettavuutta. Hyvin testattu koodi on myös helpompi ylläpitää ja päivittää tulevaisuudessa.

# Kuinka
Testien kirjoittaminen Java-ohjelmointikielellä on helppoa ja vaivatonta. Seuraavassa on esimerkki yksinkertaisesta testistä, joka tarkistaa, että annettu funktio palauttaa odotetun tuloksen:

```Java
public class Testi {

  public static void main(String[] args) {
    // Kutsutaan testattavaa funktiota ja tallennetaan tulos muuttujaan
    int tulos = laskeSumma(5, 7);
    // Tarkistetaan, että tulos on odotettu
    if (tulos == 12) {
      System.out.println("Testi läpäisty: oikea tulos saatiin!");
    } else {
      System.out.println("Testi ei läpäissyt: odotettu tulos oli 12, mutta saatiin " + tulos);
    }
  }
  
  // Yksinkertainen funktio, joka laskee kahden luvun summan
  public static int laskeSumma(int luku1, int luku2) {
    return luku1 + luku2;
  }
}
```

Tulostus:

```
Testi läpäisty: oikea tulos saatiin!
```

# Deep Dive
Testien kirjoittamisessa on hyvä noudattaa muutamia periaatteita, jotta ne olisivat mahdollisimman tehokkaita ja helppolukuisia. Ensinnäkin, on tärkeää testata rajatapauksia ja erikoistilanteita, jotta koodin virhealttius saadaan minimoitua. Lisäksi testit tulisi pitää mahdollisimman yksinkertaisina ja keskittyä vain yhteen asiaan kerrallaan. Testien tulee myös olla riippumattomia muista testeistä ja suorittua nopeasti.

Mikäli olet kiinnostunut syvemmin testien kirjoittamisesta ja miten niitä voidaan hyödyntää Java-ohjelmoinnissa, suosittelemme tutustumaan seuraaviin resursseihin:

## Katso myös
- Java Test Driven Development: https://guides.webschools.io/community-guides/testing/java/
- JUnit testing framework: https://junit.org/junit5/
- TDD tutorial: https://www.tutorialspoint.com/junit/junit_test_framework.htm