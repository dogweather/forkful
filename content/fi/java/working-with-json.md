---
title:                "Työskentely jsonin kanssa"
html_title:           "Java: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-json.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

JSON eli JavaScript Object Notation on yleisesti käytetty formaatti, joka mahdollistaa tietojen tallentamisen ja jakamisen ohjelmien välillä. Se on erityisen suosittu web-kehityksessä ja RESTful web-palveluissa. JSON:ia käytetään, koska se on helposti luettava ja kirjoitettava sekä helposti muunnettavissa eri ohjelmointikielille.

# Kuinka?

Java tarjoaa mukavan tavan käsitellä JSON-tietoja käyttämällä org.json kirjastoa. Kirjaston avulla voit helposti luoda, lukea ja muokata JSON-objekteja. Alla on esimerkki JSON-objektin luomisesta ja sen konvertoimisesta merkkijonoksi:

```Java
JSONObject obj = new JSONObject();
obj.put("nimi", "Matti Meikäläinen");
obj.put("ikä", 30);
obj.put("harrastukset", new JSONArray(Arrays.asList("luistelu", "tennis")));
String json = obj.toString();
System.out.println(json);

// Output: {"nimi":"Matti Meikäläinen", "ikä":30, "harrastukset":["luistelu", "tennis"]}
```

# Syventyminen

JSON syntyi vuonna 2001 ja on lähtenyt hallitsemaan tiedonsiirtoa webissä. Sitä käytetään laajalti mm. RESTful web-palveluissa ja siitä on tullut de facto-standardi antaa tietojen liikkua eri ohjelmointikielien välillä.

Java ei ole ainoa kieli, joka pystyy työskentelemään JSON:n kanssa. On olemassa monia muita vaihtoehtoja kuten GSON, Jackson ja JSON.simple. On tärkeää valita itsellesi sopiva kirjasto projektin tarpeiden mukaan.

JSON oli alunperin inspiroitunut JavaScriptistä, mutta on nyt laajalti tuettu useimmissa ohjelmointikielissä. Kirjastoja löytyy myös muille kielille, joten JSON-objektien käsittely on helppoa käytännössä kaikilla ohjelmointikielillä.

# Katso myös

Lue lisää JSON:sta ja sen käytöstä Java-sovelluksissa täältä:
- [Oracle:n virallinen dokumentaatio](https://www.oracle.com/technetwork/articles/java/json-1973242.html)
- [Tutoriaali JSON-kirjaston käytöstä](https://www.tutorialspoint.com/json/json_java_example.htm)
- [JSON.org](https://www.json.org/) - JSON:n virallinen verkkosivusto