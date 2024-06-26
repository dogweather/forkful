---
date: 2024-01-26 01:39:27.534775-07:00
description: "Kuinka: Otetaan esimerkiksi yksinkertainen Java-luokka, joka kaipaa\
  \ uudelleenj\xE4rjestely\xE4 huonon organisaationsa ja ep\xE4selvyytens\xE4 vuoksi."
lastmod: '2024-03-13T22:44:56.453528-06:00'
model: gpt-4-0125-preview
summary: "Otetaan esimerkiksi yksinkertainen Java-luokka, joka kaipaa uudelleenj\xE4\
  rjestely\xE4 huonon organisaationsa ja ep\xE4selvyytens\xE4 vuoksi."
title: "Koodin uudelleenj\xE4rjestely"
weight: 19
---

## Kuinka:
Otetaan esimerkiksi yksinkertainen Java-luokka, joka kaipaa uudelleenjärjestelyä huonon organisaationsa ja epäselvyytensä vuoksi.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Muita operaatioita...
    }
}
```

Uudelleenjärjestelyn jälkeen meillä on:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Muita operaatioita...
}
```

Uudelleenjärjestelyllä olemme parantaneet metodien nimiä ja parametrejä luettavuuden kannalta ja poistaneet tarpeen ehdolliselle haarakkeelle yksittäisessä metodissa. Jokainen operaatio ilmaisee nyt selkeästi tarkoituksensa.

## Syväsukellus:
Uudelleenjärjestely juontaa juurensa Smalltalk-yhteisöön, jossa korostetaan koodin luettavuutta ja oliokeskeistä suunnittelua, mutta se todella otti tuulta alleen Java-maailmassa 90-luvun lopulla ja 00-luvun alussa, erityisesti Martin Fowlerin merkittävän kirjan, "Uudelleenjärjestely: Olemassa olevan koodin suunnittelun parantaminen", julkaisun jälkeen.

Uudelleenjärjestelylle on vaihtoehtoja, kuten koodin kirjoittaminen alusta alkaen. Uudelleenjärjestelyä kuitenkin usein suositaan, koska se sisältää asteittaisia muutoksia, jotka eivät häiritse sovelluksen toiminnallisuutta.

Toteutuksen yksityiskohdat Java:lla (tai millä tahansa ohjelmointikielellä) uudelleenjärjestellessä pyörivät ymmärtämisen ympärillä koodin hajujen—koodissa olevien syvempien ongelmien indikaattoreiden—osalta. Jotkut hajut sisältävät pitkiä metodeja, suuria luokkia, toistuvaa koodia ja primitiivien liiallista käyttöä. Soveltamalla uudelleenjärjestelymalleja, kuten Metodin uuttaminen, Metodin siirtäminen tai Väliaikaisen muuttujan korvaaminen kyselyllä, kehittäjät voivat systemaattisesti puuttua näihin hajuihin varmistaen samalla, että koodi pysyy toiminnallisena koko ajan.

Automaattiset työkalut, kuten IntelliJ IDEA:n uudelleenjärjestelyn tuki tai liitännäiset Eclipseen, voivat auttaa prosessissa automatisoimalla uudelleenjärjestelyjä, kuten muuttujien, metodien ja luokkien nimeäminen uudelleen, metodien tai muuttujien uuttaminen sekä metodien tai luokkien siirtäminen eri paketteihin tai nimiavaruuksiin.

## Katso myös:
- Martin Fowlerin "Uudelleenjärjestely: Olemassa olevan koodin suunnittelun parantaminen": https://martinfowler.com/books/refactoring.html
- Uudelleenjärjestelytekniikat Refactoring.Guru -sivustolla: https://refactoring.guru/refactoring/techniques
- Automaattinen uudelleenjärjestely Eclipse:ssä: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEA:n uudelleenjärjestelyn ominaisuudet: https://www.jetbrains.com/idea/features/refactoring.html

Jokainen näistä resursseista tarjoaa joko perustan uudelleenjärjestelyn periaatteiden ymmärtämiselle tai työkaluja näiden periaatteiden käyttöönottamiseen.
