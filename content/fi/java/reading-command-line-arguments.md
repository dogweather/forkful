---
title:    "Java: Komentoriviparametrien lukeminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Java-ohjelmoijien tulisi tuntea tapa lukea komentoriviargumentteja, koska se on olennainen osa ohjelmointia ja auttaa heitä kirjoittamaan tehokkaampaa ja monipuolisempaa koodia.

## Kuinka tehdä se

Java-ohjelmoijat voivat lukea komentoriviargumentteja käyttämällä `main`-funktiota ja `args`-parametria. Tässä on yksinkertainen koodiesimerkki, joka tulostaa käyttäjän antaman komentoriviargumentin:

```java
public static void main(String[] args) {
    System.out.println("Komentoriviargumentti: " + args[0]);
}
```

Jos käyttäjä antaa komentoriviltä "java Ohjelma Hello", niin ohjelman tulostus on "Komentoriviargumentti: Hello".

## Syvällinen selvitys

Komentoriviargumenttien lukeminen antaa ohjelmoijille mahdollisuuden antaa tietoja ohjelmilleen käyttäessään niitä. Tämä on erityisen hyödyllistä, kun ohjelman halutaan ottaa vastaan erilaisia parametreja eri tilanteisiin. Komentoriviargumentit ovat myös käteviä, kun halutaan antaa tietoa tiettyjen toimintojen suorittamisesta, kuten tiedostojen luomisesta tai poistamisesta.

## Katso myös

- [Java Tutorials – Reading Command-line Arguments](https://www.tutorialspoint.com/java_programming/java_command_line_arguments.htm)
- [Oracle Java Documentation - Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)