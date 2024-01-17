---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Java: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Säännöllisten lausekkeiden käyttäminen on tärkeä osa Java-ohjelmoijan työkalupakkia. Se on tapa löytää ja manipuloida tiettyjä tekstejä helposti ja tarkasti. Ohjelmoijat käyttävät säännöllisiä lausekkeita esimerkiksi tietojen validointiin ja muokkaamiseen.

# Miten:

Java-ohjelmointikielellä on sisäänrakennettu tuki säännöllisille lausekkeille. Voit käyttää niitä esimerkiksi String-luokan matches() -metodilla. Alla on muutamia esimerkkejä säännöllisten lausekkeiden käytöstä ja niiden tulosteista:

```Java
// Tarkistetaan, onko teksti "Tervetuloa" säännöllinen lauseke "[a-zA-Z]+"
boolean match = "Tervetuloa".matches("[a-zA-Z]+");

// Tulostaa true
System.out.println(match);

// Etsitään kaikki numerot tekstistä "Ostoslista: 1. maito, 2. leipä, 3. kahvi"
// ja tulostetaan ne yksi kerrallaan
String teksti = "Ostoslista: 1. maito, 2. leipä, 3. kahvi";
Pattern pattern = Pattern.compile("[0-9]+");
Matcher matcher = pattern.matcher(teksti);
while (matcher.find()) {
    System.out.println(matcher.group());
}

// Tuloste:
// 1
// 2
// 3
```

# Syvemmälle:
Säännölliset lausekkeet ovat olleet osa ohjelmointia jo pitkään ja niitä löytyy useista eri kielistä. Java on ottanut ne käyttöön jo vuodesta 1995. Säännöllisten lausekkeiden lisäksi on olemassa myös muita tapoja käsitellä merkkijonoja, kuten String-luokan substring() -metodi.

Säännölliset lausekkeet ovat erittäin hyödyllisiä silloin, kun tekstien käsittely vaatii tarkkaa ja monimutkaista haku- ja muokkaustoimintoa. Niiden avulla voi esimerkiksi validoida käyttäjän syöttämiä tietoja ja etsiä tietyiä osia suurista tekstimassoista.

# Katso myös:
- Java String-luokan javadoc: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html
- Regular-Expressions.info sivusto: https://www.regular-expressions.info/