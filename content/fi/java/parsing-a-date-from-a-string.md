---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Java: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 
Päivämäärän jäsenteleminen merkkijonosta on tärkeä ohjelmointitaito, joka mahdollistaa päivämäärätietojen käsittelyn tietokoneella. Tämä voi olla tarpeellista esimerkiksi sovelluksissa, joissa käyttäjä antaa päivämäärän tietynlaisessa muodossa ja ohjelma tarvitsee muuttaa sen tietokoneen ymmärtämään muotoon.

## Kuinka:
Käyttämällä Javaa, voimme käyttää erilaisia ​​metodeja muuntaa merkkijono päivämääräksi. Alla on esimerkki, kuinka tämä voidaan tehdä:
```Java
 //Esimerkki päivämäärän muuntamisesta merkkijonosta
 String merkkijono = "12.11.2020";
 SimpleDateFormat muotoilu = new SimpleDateFormat("dd.MM.yyyy");
 Date paivamaara = muotoilu.parse(merkkijono);
 System.out.println(paivamaara);
```

Tämä tulostaa päivämäärän muodossa "November 12, 2020". Tässä käytetty SimpleDateFormat-luokka mahdollistaa päivämäärän muotoilun halutunlaiseksi.

## Syvällinen sukellus:
Päivämäärän jäsentelemisen tarve on syntynyt tietokoneiden käyttöönoton myötä ja erilaisten päivämäärämuotojen käytön kasvaessa. Vaihtoehtoisia tapoja muuntaa päivämäärä merkkijonosta ovat esimerkiksi käyttää apuna Regex-lausuntoja tai käyttää kirjastoja kuten Joda-Time.

Päivämäärän jäsentelemisen toteutus riippuu käytettävissä olevasta ohjelmointikielestä ja sen tarjoamista työkaluista. Java tarjoaa näppärät SimpleDateFormat-luokan, mutta muissa kielissä voi olla erilaisia vaihtoehtoja.

## Katso myös:
- [SimpleDateFormat luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Joda-Time kirjasto](https://www.joda.org/joda-time/)