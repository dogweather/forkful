---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonosta tiettyyn malliin sopivien merkkien poistaminen on tyypillinen ohjelmointitehtävä. Tämä auttaa puhdistamaan tietoja tai räätälöimään merkkijonojen esitystä.

## Kuinka:
Käytetään `replaceFirst` ja `replaceAll` metodeja `String` luokassa. Aloitetaan tekemällä merkkijonosta otos, josta haluamme poistaa numerot.

```Java
String sample = "Java, julkaistu 1995 versio 1.0.0";
String result = sample.replaceAll("\\d", "");
System.out.println(result);
```
Output:

```shell
Java, julkaistu  versio . .
```
Tässä `"\\d"` on kuvio, joka vastaa minkä tahansa numeron ja "" korvaa löydetyt numerot.

## Syvä Sukellus
Ensimmäinen versio Javasta julkaistiin vuonna 1995, ja se sisälsi `String` luokan ja `replaceFirst`, `replaceAll` metodit. Vaihtoehtoina voit käyttää `StringBuffer` tai `StringBuilder`, mutta niiden käyttäminen edellyttää useita vaiheita, joten `String` luokka on kätevämpää käyttää.

```Java
StringBuilder sb = new StringBuilder(sample);
for (int i = 0; i < sb.length(); i++) {
    if (Character.isDigit(sb.charAt(i))) {
        sb.deleteCharAt(i--);
    }
}
```
Output:

```shell
Java, julkaistu  versio . .
```
Koska Java merkkijono on muuttumaton, `replaceAll` -menetelmä palauttaa uuden merkkijonon. Vanha merkkijono on edelleen muistissa kunnes roskankerääjä poistaa sen.

## Katso myös
JavaSE-dokumentaatio merkkijonoista: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html

Oracle-dokumentaatio regexistä: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html

TutorialsPoint artikkeli Javan merkkijonoista: https://www.tutorialspoint.com/java/java_string_replaceall.htm