---
title:                "Merkkijonon interpolointi"
date:                  2024-01-20T17:51:10.398125-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon interpolointi tarkoittaa muuttujien, lausekkeiden tai arvojen upottamista merkkijonoon. Se tekee koodista siistimpää ja helpottaa dynaamisten viestien luomista.

## Kuinka:
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String user = "Jukka";
        int points = 100;

        String message = String.format("Hei %s, sinulla on %d pistettä!", user, points);
        System.out.println(message);
    }
}
```
Tulostus:
```
Hei Jukka, sinulla on 100 pistettä!
```

## Syväsukellus
Ennen Java 5:ttä merkkijonointerpolointia ei ollut suoraan kielen tukemana. Käytettiin yhdistämistä `+`:lla. Java 5 toi `String.format()`, inspiraationa C:n `printf`. Javassa 15:ssä esiteltiin tekstilohkot (JEP 378), mikä helpottaa monirivisen tekstin hallintaa.

Vaihtoehtoina ovat useat kolmannen osapuolen kirjastot, kuten Apache Commons Lang `StringUtils` ja Java-kirjasto printf-tyyliin. Nämä kirjastot voivat lisätä lisätoimintoja, mutta lisäävät riippuvuuksia.

Interpoloinnissa tärkeää on muistaa:
1. Selkeys: Interpoloinnin tulisi tehdä koodista luettavampaa.
2. Suorituskyky: Liiallinen interpoloinnin käyttö voi vaikuttaa suorituskykyyn.
3. Ylläpito: Muutos kehikossa on helpompaa, jos merkkijonot ovat keskitetysti hallinnassa.

## Näe Myös
- [Java String.format documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [JEP 378: Text Blocks (Final)](https://openjdk.java.net/jeps/378)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
