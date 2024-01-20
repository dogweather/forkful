---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Uuden projektin aloittaminen on tyhjästä luomista, ohjelman tai sovelluksen rakentaminen alusta alkaen. Ohjelmoijat tekevät sen ratkaistakseen ongelman, vastatakseen tarpeeseen tai opetakseen uutta teknologiaa.

## Näin se tehdään:
Luo uusi Java-projekti käyttämällä komentoa "javac":

```Java
$ javac HelloWorld.java
```
Tämä kompiloi Java-lähdekooditiedoston (HelloWorld.java) ja luo luokkatiedoston (HelloWorld.class). Ajetaan ohjelma komentoriviltä näin:

```Java
$ java HelloWorld
```
Tuloste pitäisi olla:

```Java
Hello, World!
```

## Sukellus syvälle:
Uuden projektin aloittaminen on ollut Java-ohjelmoijien perinteinen tehtävä alusta asti. Alternatiiveiksi voi esimerkiksi ottaa käyttöön Maven tai Gradle, ne ovat projektinhallintatyökaluja, jotka voivat helpottaa prosessia ja tarjota lisää toiminnallisuuksia. Uuden projektin aloittaminen sisältää tavallisesti uuden projektikansion luomisen, uuden lähdekooditiedoston luomisen, lähdekoodin kirjoittamisen ja kompileerimisen sekä ohjelman ajamisen.

## Katso myös:
- [Java SE -opas](https://docs.oracle.com/en/java/javase/index.html)
- [Mavenin aloitusopas](https://maven.apache.org/guides/getting-started/)
- [Gradlen dokumentaatio](https://docs.gradle.org/current/userguide/userguide.html)