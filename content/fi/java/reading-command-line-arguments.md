---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Java: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi lukea komentorivin argumentteja Java-ohjelmoinnissa? Yksi syy voi olla tarve saada käyttäjältä tietoja ohjelman suorituksen aikana. 

## Kuinka

Usein ohjelmat vaativat tiettyjä arvoja tai vaihtoehtoja tulkittavaksi ja käsiteltäväksi. Tämä voidaan tehdä helposti hyödyntäen komentorivin argumentteja. Katso alla oleva esimerkki, jossa luodaan ohjelma, joka tulostaa käyttäjän antaman parametrin mukaisen viestin.

```java 
public class ArgumentExample {

  public static void main(String[] args) {

    if (args.length == 0) {
      System.out.println("Anna parametri tulostettavalle viestille!");
    } else {
      System.out.println("Käyttäjän antama parametri: " + args[0]);
    }

  }
}
```

Esimerkki syöte ja tuloste:

```bash 
java ArgumentExample Hello
Käyttäjän antama parametri: Hello
```

## Syvemmälle

Komentorivin argumentit voidaan lukea Java-ohjelmassa hyödyntämällä `args`-muuttujaa, joka on `String`-taulukko. Tämä taulukko sisältää kaikki parametrit, jotka on annettu ohjelman käynnistyksen yhteydessä. Voit myös käyttää `foreach`-silmukkaa helpottamaan argumenttien käsittelyä, kuten alla olevassa esimerkissä:

```java
public class ArgumentLoopExample {

  public static void main(String[] args) {

    for (String arg : args) {
      System.out.println("Käyttäjän antama parametri: " + arg);
    }

  }
}
```

Esimerkki syöte ja tuloste:

```bash
java ArgumentLoopExample Hello World
Käyttäjän antama parametri: Hello
Käyttäjän antama parametri: World
```

## Katso myös

- [Oracle:n Java dokumentointi: Komentorivin argumentit](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Baeldung:n artikkeli: Java Command-Line Arguments Parsing](https://www.baeldung.com/java-command-line-arguments-parsing)