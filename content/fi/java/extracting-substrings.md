---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Aloitus Java: Opi Alistringien Poisto

## Mitä & Miksi?
Substringien poisto on toiminto, jossa uusi merkkijono luodaan olemassa olevasta merkkijonosta. Ohjelmoijat tekevät tämän esimerkiksi silloin, kun he tarvitsevat tietyn osan merkkijonosta erilliseen käyttöön tai analysointiin.

## Kuinka Tehdään:
```java
public class Main {
    public static void main(String[] args) {
        String str = "Moikka Suomesta!";
        String sub = str.substring(7, 15); 
        System.out.println(sub); 
    }
}
```
Näytteellä on tulostus:
```
Suomesta!
```
Tässä `substring()`-metodi ottaa kaksi parametria: alkuindeksin ja loppuindeksin. Alkuindeksi on paikan ensimmäinen sijainti, josta aloitetaan ja loppuindeksi on sijainti johon asti luetaan.

## Syvemmälle Sukellus
Historiallisesti substring-toiminto löytyy useista ohjelmointikielistä ja sillä on pitkä käytännön historia. Vaihtoehtoisesti voit käyttää `subSequence()`, joka on periaatteessa sama metodi, mutta se palauttaa `CharSequence`-olion. Interneissä yksityiskohdissa `substring()`-luokka tekee sisäistä optimointia vähentämään muistin käyttöä, kun luodaan useita alimerkkijonoja samasta alkuperäisestä merkkijonosta.

## Katso Myös
Java String substring() metodi: [Oracle Java Dokumentaatio](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
StringManipulation Java: [GeeksforGeeks](https://www.geeksforgeeks.org/string-class-in-java/)  

Kun olet täysin ymmärtänyt substringien luomisen merkityksen ja kuinka se toimii, voit tutustua samankaltaisista toiminnoista `split()`, `concat()`, ja `trim()`, jotka myös kuuluvat Java String -luokkaan.