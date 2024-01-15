---
title:                "Päällekkäisen kuvion poisto merkeistä"
html_title:           "Java: Päällekkäisen kuvion poisto merkeistä"
simple_title:         "Päällekkäisen kuvion poisto merkeistä"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Jokaisessa ohjelmoinnin kielestä löytyy tilanteita, joissa halutaan poistaa merkkejä, jotka vastaavat tiettyä kaavaa tai kuvioa. Tämä artikkeli käsittelee, miten tämä voidaan tehdä Javan avulla ja miksi se voi olla hyödyllistä.

## Miten

Oletetaan, että meillä on merkkijono "Hello, world!". Haluamme poistaa kaikki välilyönnit tästä merkkijonosta. Käytämme ```Java
replace()``` metodia ja annamme sille kaksi parametria: välilyönti (" ") ja tyhjä merkkijono (""). Tämä korvaa kaikki välilyönnit tyhjillä merkkijonoilla.

```
String teksti = "Hello, world!";
String uusiTeksti = teksti.replace(" ", "");
System.out.println(uusiTeksti);
```
Tulos olisi: "Hello,world!". Välilyönnit on nyt poistettu!

Toinen esimerkki voisi olla poistaa kaikki numerot merkkijonosta. Käytämme tällöin säännöllisiä lausekkeita (regular expressions) ja ```Java
replaceAll()``` metodia:

```
String teksti = "Hell0, w0rld!";
String uusiTeksti = teksti.replaceAll("[0-9]", "");
System.out.println(uusiTeksti);
```

Tulos olisi: "Hell, wrld!". Kaikki numerot on nyt poistettu merkkijonosta.

## Syvällisemmin

Javan String-luokka tarjoaa monia hyödyllisiä metodeja merkkijonojen käsittelyyn. Yksi näistä on ```Java
replace()```, joka korvaa tietyn merkkijonon toisella merkkijonolla. Toinen hyödyllinen metodi on ```Java
replaceAll()```, joka käyttää säännöllisiä lausekkeita merkkijonon korvaamiseen.

Säännölliset lausekkeet ovat merkkijonoja, jotka kuvaavat tiettyä kaavaa tai kuvioa. Niitä voi käyttää laajasti merkkijonojen käsittelyssä, esimerkiksi poistamalla tiettyjä merkkejä tai löytämällä tiettyjä merkkijonoja. Java tarjoaa säännöllisiä lausekkeita varten luokan ```Java
Pattern``` ja sen avulla voi luoda haluttuja kaavoja.

## Katso myös

- [Virallinen Java-dokumentaatio](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#replace(java.lang.CharSequence,java.lang.CharSequence))
- [W3Schools esimerkkejä merkkijonojen manipuloinnista Javalla](https://www.w3schools.com/java/java_string.asp)