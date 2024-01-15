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

## Miksi

Kun ohjelmoimme Javaa, yksi yleinen tehtävä on työskennellä JSON-tiedostojen kanssa. JSON (JavaScript Object Notation) on yksi yleisimmistä tavoista tallentaa ja siirtää tietoja sovellusten välillä. Tässä artikkelissa opit, miten käsitellä JSON-tiedostoja Java-koodissa.

## Miten

JSON-tietojen käsittely Javassa vaatii käyttöön JSONObject- ja JSONArray-luokat, jotka löytyvät org.json-paketista. Alla on esimerkki koodista, joka luo JSON-tiedoston ja lisää sinne tietoja:

```Java
JSONObject json = new JSONObject();
json.put("nimi", "Matti");
json.put("ikä", 25);
JSONArray harrastukset = new JSONArray();
harrastukset.put("jalkapallo");
harrastukset.put("valokuvaus");
json.put("harrastukset", harrastukset);
```

Tämä koodi luo JSON-tiedoston, jossa on Matti-niminen henkilö, joka on 25-vuotias ja hänellä on kaksi harrastusta. Voit myös lukea ja muokata JSON-tiedostoja käyttämällä JSONObject- ja JSONArray-luokkien metodeja.

## Syventyminen

Tässä muutama lisätieto JSON-tietojen käsittelystä Javassa:

- Voit ladata JSON-tiedoston ulkoisesta lähteestä (kuten URL:stä) käyttämällä JSONTokener-luokkaa ja sen constructoria, joka ottaa parametrinä Inputstreamin.
- Voit luoda JSON-tiedoston käyttämällä JSONObject-luokan constructoria, joka ottaa parametrinä JSON-merkkijonon.
- Voit luoda JSON-tiedoston käyttämällä JSONObject-luokan constructoria, joka ottaa parametrinä Map-olion, jossa on avain-arvo-pareja.
- Voit luoda JSON-tiedoston käyttämällä JSONObject-luokan constructoria, joka ottaa parametrinä JSON-tiedoston nimen, ja JSONObject tallentaa sen sisällön String-tyyppiseen muuttujaan.

## Katso myös

- [JSON.org](https://www.json.org/json-fi.html) - Lisätietoa JSON-formaatista ja sen käytöstä.
- [JSON-lukija ja kirjoittaja](https://www.baeldung.com/java-org-json) - Vaivaton opas JSON-tiedostojen lukemiseen ja kirjoittamiseen Javassa.
- [JSON Maven Repository](https://mvnrepository.com/artifact/org.json/json) - JSON-paketti, jonka voit lisätä Maven-projektiisi.