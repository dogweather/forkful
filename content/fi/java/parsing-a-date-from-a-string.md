---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tietojen hahmontaminen (eli parsiminen) tekstistä on prosessi, jossa jäsennellään erityisessä formaatissa oleva tekstiä ja muunnetaan se sopivaan esitysmuotoon, kuten Java-päivämääräksi. Tätä käytetään usein, kun käsitellään ulkoista dataa, esimerkiksi API-tiedusteluja tai tiedostojen lukemista.

## Näin tehdään:
Aloitetaan perus esimerkillä kuinka parsiaan päivämäärä String-tyyppiseltä arvolta:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        String pvmTeksti = "2022-02-01";
        DateTimeFormatter muodonMuunnin = DateTimeFormatter.ofPattern("yyyy-MM-dd");
       
        LocalDate parsedPvm = LocalDate.parse(pvmTeksti, muodonMuunnin);
        System.out.println("Parsed päivämäärä: " + parsedPvm);
    }
}
```

Tämän ohjelman suoritus tuottaa tulosteen: `Parsed päivämäärä: 2022-02-01`.

## Syvällinen tarkastelu
Historiallisesti päivämäärä-parsing oli suoritettu `SimpleDateFormat` luokalla, mutta se on ollut heikoilla kohdallaan syyt johtuen turvallisuusongelmista ja suorituskyvystä. Nykyaikaisempi tapa on käyttää `java.time` kirjastoa, jota on käytetty tässä esimerkissä.

On olemassa myös muita vaihtoehtoja, kuten Joda-Time kirjasto, mutta `java.time` on nyt suositumpaa koska se on osa standardia Java-kirjastoa.

Java-päivämäärän hahmontaminen vaatii jäsennyskuvion. Yleisimmät kuviot ovat `yyyy-MM-dd` päivämäärille ja `HH:mm:ss` ajoille. On tärkeää muistaa, että kuukauden ja minuutin kuvioiksi käytetään suurta M-kirjainta ja pientä m-kirjainta.

## Katso myös
Ohjelmistoja ja esimerkkejä, jotka saattavat auttaa sinua ymmärtämään tätä paremmin:
- [Joda-Time - Java date and time API](https://www.joda.org/joda-time/)
- [Java DateTimeFormatter luokka](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Miksi SimpleDateFormat ei ole thread-safe](https://stackoverflow.com/questions/6840803/why-is-javas-simpledateformat-not-thread-safe)