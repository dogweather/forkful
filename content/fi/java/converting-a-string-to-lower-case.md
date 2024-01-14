---
title:                "Java: Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? Yksi syy voisi olla, että haluat vertailla merkkijonoja ilman, että isot ja pienet kirjaimet vaikuttavat vertailuun. Toinen syy voisi olla, että käyttäjät syöttävät tietoja eri tavoin ja haluat muuttaa ne yhtenäiseen muotoon esimerkiksi tietokantaa tai hakutoimintoa varten.

## Näin teet sen

Merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa Java-ohjelmoinnissa. Käytä yksinkertaisesti `.toLowerCase()`-metodia merkkijonon perässä. Tässä on esimerkki koodista:

```Java
String s = "TÄMÄ ON MERKKIJONO";
String lowerCase = s.toLowerCase();
System.out.println(lowerCase);
```

Tämä tulostaisi "tämä on merkkijono" konsolille. Huomaa, että alkuperäisen merkkijonon arvo ei muutu, vaan `.toLowerCase()`-metodi palauttaa uuden merkkijonon.

## Syvällisemmin

Merkkijonon muuttaminen pieniksi kirjaimiksi perustuu Unicode-standardiin. Jokaisella kirjaimella on omaa vastaava pieni kirjain ja `.toLowerCase()`-metodi käyttää tätä tietoa muuttaessaan merkkijonon kirjaimia.

On myös huomioitava, että kun käytetään esimerkiksi suomen kieltä, niin tiettyjen kirjainten muuttaminen pieniksi kirjaimiksi voi antaa erilaisen lopputuloksen kuin signaalia käytettäessä. Tämä johtuu siitä, että suomen kielen aakkoset eivät ole samassa järjestyksessä kuin englannin kielen vastaavat kirjaimet.

## Katso myös

- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Unicode-taulukko](http://unicode.org/charts/)