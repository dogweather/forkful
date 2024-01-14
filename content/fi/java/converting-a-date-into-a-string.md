---
title:                "Java: Päiväyksen muuntaminen merkkijonoksi"
simple_title:         "Päiväyksen muuntaminen merkkijonoksi"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaisi muuttaa päivämäärän merkkijonoksi? On monia syitä, kuten päivämäärän tallentaminen tietokantaan, tulostaminen käyttäjälle tai vertaaminen muihin päivämääriin.

## Miten
``` java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringExample{
  public static void main(String[] args){
    //luodaan päivämääräolio
    Date date = new Date();
    
    //määritetään haluttu muoto
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
    
    //muutetaan päivämäärä merkkijonoksi
    String dateAsString = formatter.format(date);
    
    //tulostetaan muunnettu merkkijono
    System.out.println(dateAsString);
  }
}

```
**Lähtö**\
25/10/2021

## Syvällinen sukellus
Java tarjoaa useita erilaisia tapoja muuttaa päivämäärä merkkijonoksi. SimpleDateFormat-luokka antaa mahdollisuuden määrittää haluttu muoto, kuten päiväys, kuukausi ja vuosi eri järjestyksessä. Lisäksi voit käyttää toisen luokan, kuten DateTimeFormatterin, muuttamaan päivämäärän merkkijonoksi eri tavoin. On myös tärkeää ottaa huomioon aikavyöhyke, kun muunnat päivämäärän merkkijonoksi, jotta oikea aika voidaan näyttää käyttäjälle.

## Katso myös
- [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [DateTimeFormatter-luokka](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Päivämäärän muuntaminen merkkijonoksi Java-ohjelmassa](https://www.baeldung.com/java-date-to-string)