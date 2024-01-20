---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Löydä merkkijonosi pituus Java-koodauksessa

## Mitä & Miksi?

Merkkijonon pituuden löytäminen tarkoittaa merkkisolmujen määrän laskemista merkkijonossa. Tätä toimintoa tarvitaan usein ohjelmistokehityksessä, esimerkiksi validoinnissa tai silmukoissa.

## Miten:

Aloitetaan yksinkertaisimmalla tavalla merkkijonon pituuden selvittämiseksi Java-koodauksessa.

```Java
public class Main {
    public static void main(String[] args) {
        String merkkijono = "Moi Maailma";
        int pituus = merkkijono.length();
        System.out.println("Merkkijonon pituus on: " + pituus);
    }
}
```
Tuloste:

```
Merkkijonon pituus on: 11
```

## Sukella syvemmälle

**Historiallinen tausta**: Java-ohjelmointikielen `length()`-metodi on ollut olemassa ohjelmointikielen ensimmäisistä versioista asti. Tämä metodi on perusta monille ohjelmointitehtäville.

**Vaihtoehdot**: Halutessasi voit myös laskea merkkijonon pituuden käyttämällä Java 8 -virran metodia, kuten alla esitetyssä esimerkissä:

```Java
public class Main {
    public static void main(String[] args) {
        String merkkijono = "Moi Maailma";
        long pituus = merkkijono.chars().count();
        System.out.println("Merkkijonon pituus on: " + pituus);
    }
}
```

**Toteutuksen yksityiskohdat**: `length()`-metodi on määritetty `String` luokassa, joka palauttaa `int` tyyppisen arvon, joka on merkkijonon pituus. Jos merkkijono on null tai tyhjä, `length()` palauttaa arvon 0.

## Katso myös:

2. [Java String -luokka (Oracle Docs)](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)

Syvempiä tietoja ja lisää esimerkkejä yllä olevista linkeistä. Opettele, koodaa ja kehitä taitojasi!