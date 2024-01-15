---
title:                "Merkkijonon konvertointi pieniksi kirjaimiksi"
html_title:           "Java: Merkkijonon konvertointi pieniksi kirjaimiksi"
simple_title:         "Merkkijonon konvertointi pieniksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi?

Tekstisyöttö on erittäin yleistä ohjelmoinnissa ja joskus on tarpeen muuttaa annettu merkkijono pieniksi kirjaimiksi. Tässä ohjeessa opetan sinulle, kuinka voit helposti muuttaa merkkijonon pieniksi kirjaimiksi Java-ohjelmoinnissa.

## Kuinka?

```Java 
String s = "TEKSTISYÖTTÖ";
String lowerCase = s.toLowerCase();

System.out.println(lowerCase); // tulostaa "tekstisyöttö"
```

1. Aloita luomalla muuttuja, joka sisältää merkkijonon, jota haluat muuttaa pieniksi kirjaimiksi.
2. Käytä `toLowerCase()` -metodia, joka muuttaa merkkijonon pieniksi kirjaimiksi ja tallentaa sen uuteen muuttujaan.
3. Tulosta uusi muuttuja `System.out.println()` -metodilla.

## Syvempi sukellus

`toLowerCase()` -metodi kuuluu `String` -luokkaan ja se muuttaa kaikki merkit alkuperäisessä merkkijonossa pieniksi kirjaimiksi. Tämä on erittäin hyödyllinen, kun haluat vertailla merkkijonoja, koska se poistaa mahdolliset erot kirjainten kirjoitusasuissa.

Huomaa, että tämä metodi on kielestä riippumaton, joten se toimii myös muiden kielten kuin suomen kanssa.

## Katso myös

- [`toUpperCase()` -metodi Java-dokumentaatiossa](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#toLowerCase--)
- [StackOverflow-ketju merkkijonon muuttamisesta pieniksi kirjaimiksi Java-ohjelmoinnissa](https://stackoverflow.com/questions/26512375/case-insensitive-string-comparison-java)