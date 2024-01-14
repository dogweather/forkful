---
title:                "Java: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Yhä useampi Java-ohjelmoija käyttää JSON-muotoa tiedon siirtoon ja tallentamiseen sovelluksissaan. JSON on nopea, kevyt ja helppokäyttöinen tapa käsitellä tietoa sovelluksissa. Se on myös standardi useissa verkkoteknologioissa, kuten RESTful API: en toteutuksessa. Näiden syiden vuoksi on tärkeää, että Java-ohjelmoijat ymmärtävät JSON-rakenteen ja osaavat käyttää sitä tehokkaasti.

## Kuinka

JSON-rakenteen käsittely Java-sovelluksissa on helppoa ja suoraviivaista. Tässä on muutama esimerkki, joiden avulla pääset alkuun:

### Oman JSON-objektin luominen

Java-koodissa voit luoda uuden JSONObject-olion ja lisätä siihen haluamasi avaimet ja arvot.

```Java
JSONObject obj = new JSONObject();
obj.put("nimi", "Matti");
obj.put("ikä", 35);
obj.put("siviilisääty", "naimisissa");
System.out.println(obj);
```

Tämän esimerkin tulostus näyttää seuraavalta:

```
{"nimi":"Matti","ikä":35,"siviilisääty":"naimisissa"}
```

### JSON-tietojen lukeminen tiedostosta

Voit myös ladata JSON-tiedoston ja lukea sen sisältämän datan Java-sovelluksessa. Tässä esimerkissä käytämme apuna JSONObject- ja JSONArray-luokkia.

```Java
// Luodaan uusi JSONObject lukemalla tiedosto "tiedosto.json"
JSONObject obj = new JSONObject(new File("tiedosto.json"));
// Haetaan objektista avain "sijainti" ja tulostetaan sen arvo
System.out.println(obj.getString("sijainti"));

// Luetaan "käyttäjät" avaimen arvot taulukkoon
JSONArray kayttajat = obj.getJSONArray("käyttäjät");
// Loopataan taulukon läpi ja tulostetaan jokaisen käyttäjän nimi
for (int i = 0; i < kayttajat.length(); i++) {
    System.out.println(kayttajat.getJSONObject(i).getString("nimi"));
}
```

Esimerkin tulostus:

```
Espoo
Matti
Anna
```

Tämä on vain pintaraapaisu JSON:n käyttöön Java-sovelluksissa. Voit lukea lisää mahdollisuuksista JavaDocista tai alla olevista linkeistä.

## Syvällinen sukellus

JSON-rakenteen ymmärtäminen auttaa ohjelmoijia käsittelemään ja siirtämään dataa tehokkaasti sovelluksissaan. Tärkeimmät asiat, jotka on hyvä tietää JSON:sta, ovat:

- Avaimet ja arvot: JSON-objekti koostuu avain-arvo -pareista, joissa avain on tekstiä ja arvo voi olla esimerkiksi numero, merkkijono, toinen objekti tai taulukko.
- Taulukot: JSON:ssa voidaan käyttää myös taulukoita, jotka koostuvat eri objekteista tai arvoista.
- Syntaksi: JSON käyttää yksinkertaista syntaksia, jossa tiedot ovat joko merkkijonona, numeroina, boolean-arvoina tai null-arvona.

## Katso myös

- [JSON - Wikipedia](https://fi.wikipedia.org/wiki/JSON)
- [JavaDoc - JSONObject-luokka](https://docs.oracle.com/javase/7/docs/api/org/json/JSONObject.html)
- [JSON-tiedostomuoto - tuettujen tietotyyppien luettelo](https://www.json.org/json-fi.html)