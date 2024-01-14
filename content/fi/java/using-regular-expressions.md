---
title:    "Java: Säännöllisten lausekkeiden käyttö"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Javassa?

Säännölliset lausekkeet ovat voimakas työkalu, joka mahdollistaa monimutkaisten tekstien etsimisen ja muokkaamisen Javassa. Niiden avulla voit suorittaa tarkkoja hakuja ja korvauksia yhdellä yksinkertaisella koodirivillä. Ne ovat erityisen hyödyllisiä, kun sinun on käsiteltävä suuria tekstimääriä, kuten tiedostoja tai käyttäjän antamia syötteitä.

## Miten käyttää säännöllisiä lausekkeita Java-koodissa?

Säännöllisiin lausekkeisiin liittyvät Javan luokat ja metodit löytyvät paketista `java.util.regex`. Sinun on tuotava tämä paketti sisällyttääksesi ne koodiisi. 

### Haku ja korvaus

Yksi yleisimmistä käyttötarkoituksista säännöllisille lausekkeille Javassa on tekstien haku ja korvaus. Voit käyttää `Pattern` ja `Matcher` -luokkia tekstin hakuun ja siihen liittyviin korvauksiin.

```Java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Tämä on esimerkkiteksti, jossa on muutama luku 123 ja 456.";
        String regex = "\\d+"; // Haku kaikista numeroista
        String korvaus = "XXX"; // Korvausmerkintä

        // Luo pattern-olio
        Pattern pattern = Pattern.compile(regex);
        
        // Suorita haku tekstistä
        Matcher matcher = pattern.matcher(text);

        // Korvaa kaikki numerot merkillä XXX ja tulosta tulos
        String korvattuTeksti = matcher.replaceAll(korvaus);
        System.out.println(korvattuTeksti);
    }
}

```
**Tulostaa:** Tämä on esimerkkiteksti, jossa on muutama luku XXX ja XXX.

### Split ja match-esimerkki

Voit myös käyttää säännöllisiä lausekkeita tekstin erottamiseen `split` -metodin avulla. Voit käyttää myös säännöllisiä lausekkeita tekstin vertaamiseen `matches` -metodilla.

```Java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Hello,world! Welcome-to Java.";
        String regex = "[!,-]"; // Halkaisija huutomerkistä tai viivasta
        String[] osat = text.split(regex); // Osaa sanoja huutomerkistä tai viivasta

        // Tulosta osat
        for (String osa : osat) {
            System.out.println(osa);
        }

        // Teksti vastaa ehtoa
        if (text.matches(".*Hello.*Java.*")) {
            System.out.println("Teksti sisältää Hello ja Java!");
        } else {
            System.out.println("Teksti ei sisällä Hello ja Java.");
        }
    }
}

```
**Tu