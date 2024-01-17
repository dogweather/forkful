---
title:                "Kuvion mukaisesti sopivien merkkien poistaminen"
html_title:           "Java: Kuvion mukaisesti sopivien merkkien poistaminen"
simple_title:         "Kuvion mukaisesti sopivien merkkien poistaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon yhteensovittaminen poistaa kaikki merkit, jotka vastaavat tiettyä kaavaa. Tämä on hyödyllinen työkalu ohjelmoijille, jotka haluavat nopeasti ja tehokkaasti muokata merkkijonoja.

## Miten:
Seuraavat Java-esimerkit näyttävät, kuinka voit poistaa merkkejä, jotka vastaavat haluttua kaavaa:

```
// Poistaa kaikki numeromerkit
String s = "Hei 123 maailma!";
String s2 = s.replaceAll("[0-9]", "");
System.out.println(s2); // Hei maailma!
```
```
// Poistaa kaikki välimerkit
String s = "Tervetuloa, maailma!";
String s2 = s.replaceAll("[,.!?]", "");
System.out.println(s2); // Tervetuloa maailma
```

## Syvällinen sukellus:
Merkkijonon yhteensovittamisen käsite tulee sanoista "regular expression" ja se on käytännössä jäsentelykieli, joka määrittää merkkijonojen rakenteen ja säännöt. Tämä tekniikka on ollut käytössä jo vuosikymmeniä ja on vakiinnuttanut paikkansa ohjelmoinnin työkalupakissa. On myös muita tapoja poistaa merkkejä kuten substring-metodi, mutta merkkijonon yhteensovittaminen on yleisesti pidetty tehokkaampana ja monipuolisempana vaihtoehtona.

## Katso myös:
- [Java Regex Tutorial](https://www.baeldung.com/java-regex)
- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)