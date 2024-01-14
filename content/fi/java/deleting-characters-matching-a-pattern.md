---
title:                "Java: Pohjautuvien merkkien poistaminen"
simple_title:         "Pohjautuvien merkkien poistaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointitehtävissä saatat törmätä tilanteeseen, jossa haluat poistaa tietyn kaavan mukaiset merkit merkkijonosta. Tämä voi olla hyödyllistä esimerkiksi datan käsittelyssä tai tietojen suodattamisessa. Jatka lukemista oppiaksesi, miten voit toteuttaa tämän Java-koodilla.

## Miten

Java tarjoaa mahdollisuuden käyttää regular expression -tekniikkaa, joka on erittäin hyödyllinen merkkijonojen käsittelyssä. Voit käyttää metodia `replaceAll()` poistamaan merkkijonon osia, jotka vastaavat haluamaasi kaavaa. Alla on esimerkki koodista, joka poistaa kaikki numerot sisältävät merkit:

```Java
String originalString = "Hello123";
String modifiedString = originalString.replaceAll("[0-9]", "");
System.out.println(modifiedString);
```

Tämän koodin tuloste olisi `Hello`.

Voit myös käyttää `matches()` -metodia tarkistamaan, vastaako merkkijono haluamaasi kaavaan. Alla olevassa esimerkissä tarkistetaan, sisältääkö merkkijono ainoastaan kirjaimia:

```Java
String stringToCheck = "HelloWorld";
if (stringToCheck.matches("[a-zA-Z]+")) {
  System.out.println("Merkkijono koostuu ainoastaan kirjaimista.");
}
```

Tämä koodi tulostaisi `Merkkijono koostuu ainoastaan kirjaimista.`.

## Syväsyvennys

Regular expression -tekniikka tarjoaa monia erilaisia merkintöjä, joita voit käyttää määrittelemään haluamasi kaavan. Esimerkiksi `[0-9]` vastaa kaikkia numeroita ja `[a-zA-Z]` vastaa kaikkia kirjaimia. Voit myös käyttää `+`-merkintää vastaamaan yhtä tai useampaa edeltävää merkkiä.

On myös tärkeää muistaa, että `replaceAll()`-metodi palauttaa uuden merkkijonon, joten sinun täytyy tallentaa se uuteen muuttujaan jos haluat käyttää sitä myöhemmin. `replaceAll()` ja `matches()`-metodeilla voit myös käyttää säännöllistä lausetta ($regular expression$) muuttujana.

## Katso myös

- [The Java Tutorials - Reglar Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular Expression Cheatsheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Matcher Class Documentation](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html)