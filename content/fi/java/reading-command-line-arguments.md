---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komentoriviparametrien lukeminen Java-ohjelmassa tarkoittaa tietojen saamista käyttäjältä suorituksen yhteydessä. Tämä on hyödyllistä, koska se antaa ohjelmalle dynaamisuutta ja mukautuvuutta.

## Kuinka:

Java-sovelluksissa komentoriviparametrit luetaan pääohjelman argumenttina. Katsotaan esimerkkiä tästä:

```java
public class Main {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println("Argumentti: " + arg);
        }
    }
}
```

Kun suoritat tämän ohjelman komentorivilta esimerkiksi näin: `java Main hola mundo`, se tulostaa:

```java
Argumentti: hola
Argumentti: mundo
```

## Syvennys

Komentoriviparametreja on käytetty ohjelmoinnissa jo pitkään, ja ne ovat olleet oleellisia osa UNIX-tyyppisten käyttöjärjestelmien käytössä.

Java tarjoaa yksinkertaisen tavan käsitellä näitä parametreja "args"-taulukon kautta. Muita tapoja saada tietoja käyttäjältä ovat esimerkiksi Scanner-luokka tai BufferedReader.

"args" on string-tyyppisten arvojen taulukko. Se saa arvonsa ohjelman suorituksen yhteydessä komentoriviltä. Jokainen komentoriviparametri on erillinen merkkijono.

## Lisätietoa

Lisätietoja komentoriviparametrien lukemisesta löydät seuraavista lähteistä:

- Oraclein Java-tutoriaali: [Linkki](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Rajeev Kumar Singhin blogi: [Linkki](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- Stack Overflow, lisätietoa ja esimerkkejä: [Linkki](https://stackoverflow.com/questions/890966/what-is-string-args-parameter-in-main-method-java)