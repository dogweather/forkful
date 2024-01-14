---
title:                "Java: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä osa Java-ohjelmointia, sillä se mahdollistaa käyttäjien syötteiden ottamisen dynaamisesti suoritettaviin ohjelmiin. Tässä blogikirjoituksessa käymme läpi, miten voit lukea komentoriviltä syötettyjä parametreja ja kuinka ne voidaan hyödyntää Java-ohjelmoinnissa.

## Miten

Komentoriviparametrien lukeminen Java-ohjelmassa on helppoa käyttämällä `args`-parametria `main`-metodissa. Tämä parametri saa arvokseen taulukon, jossa on kaikki komentoriviltä annetut syötteet. Voit lukea taulukon arvoja käyttämällä indeksejä, kuten esimerkissä alla:

```java
public static void main(String[] args) {
    for (int i = 0; i < args.length; i++) {
        System.out.println("Parametri " + i + " : " + args[i]);
    }
}
```

###### Syöte:

```
java Ohjelma ensimmäinen toinen -optio 123
```

##### Tuloste:

```
Parametri 0 : ensimmäinen
Parametri 1 : toinen
Parametri 2 : -optio
Parametri 3 : 123
```

Kuten nähdään esimerkistä, komentoriviparametrit järjestetään taulukkoon oikeassa järjestyksessä ja niitä voi käsitellä kuten mitä tahansa taulukkoa Java-ohjelmassa.

## Syvemmälle

Komentoriviparametrien lukeminen on hyödyllistä esimerkiksi silloin, kun halutaan suorittaa ohjelma eri parametreilla eri tuloksilla. Tällöin ohjelman käynnistämistä voidaan muokata dynaamisesti käyttäjän syötteiden avulla.

Lisäksi komentoriviparametreilla voidaan välittää ohjelmalle tietoa esimerkiksi tiedostojen sijainnista tai erilaisista asetuksista, jolloin ohjelma voi toimia joustavasti eri tilanteissa.

## Katso myös

- [Oracle Java - Komentoriviparametrit](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorials Point - Komentoriviparametrit Java-ohjelmassa](https://www.tutorialspoint.com/command-line-arguments-in-java)
- [Javatpoint - Komentoriviparametrien esimerkkejä](https://www.javatpoint.com/command-line-argument)