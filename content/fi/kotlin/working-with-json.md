---
title:                "Kotlin: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Monet modernit sovellukset käyttävät JSON-muotoa tietojen tallentamisessa ja siirtämisessä. JSON on myös erittäin suosittu käytännön ohjelmointikielessä, kuten Kotlin. Tästä syystä on tärkeää ymmärtää, kuinka JSON toimii ja kuinka sitä käytetään Kotlinissa.

## Kuinka

JSON-tietojen käsittely Kotlinissa on melko yksinkertaista, ja tässä on joitain käytännön esimerkkejä siitä, kuinka voit käyttää sitä omassa koodissasi. Kaikki esimerkit on kirjoitettu käyttäen Kotlinin virallista Google Gson-kirjastoa JSON-muunnokseen.

### JSON-muunnosten tekeminen

```Kotlin
// Luo yksinkertainen JSON-objekti
val jsonObject = JsonObject()
// Lisää arvoja objektiin
jsonObject.add("nimi", "Matti")
jsonObject.add("ika", 25)
jsonObject.add("tyopaikka", "Ohjelmoija")

// Muunna JSON-objekti merkkijonoksi ja tulosta se
println(jsonObject.toString())

// Output: {"nimi":"Matti","ika":25,"tyopaikka":"Ohjelmoija"}
```

### JSON-tietojen haku

```Kotlin
// Luo JSON-muoto dataa
val jsonString = "{\"nimi\":\"Matti\",\"ika\":25,\"tyopaikka\":\"Ohjelmoija\"}"

// Muunna merkkijono JSON-objektiksi
val jsonObject = JsonParser().parse(jsonString).asJsonObject

// Hae nimi ja tulosta se
val nimi = jsonObject.get("nimi").asString
println(nimi)

// Output: Matti
```

### JSON-taulukoiden käsittely

```Kotlin
// Luo JSON-muotoinen taulukko
val jsonArray = JsonArray()
// Lisää tietueita taulukkoon
jsonArray.add("Ensimmäinen")
jsonArray.add("Toinen")
jsonArray.add("Kolmas")

// Muunna taulukko merkkijonoksi ja tulosta se
println(jsonArray.toString())

// Output: ["Ensimmäinen","Toinen","Kolmas"]
```

## Syvempää tietoa

JSON-muoto on rakenteeltaan hyvin yksinkertainen, ja sen avulla voidaan tallentaa monenlaisia tietoja. JSON-puu koostuu avaimista ja niiden arvoista. Kotlinin Gson-kirjasto tarjoaa kätevän tavan muuntaa JSON-dataa merkkijonoista olioiksi ja päinvastoin.

On myös tärkeää huomata, että JSON on erittäin suosittu dataformaatti modernien sovellusten rajapinnoissa. Tämä tarkoittaa, että ymmärtämällä JSON-muotoa ja sen käsittelyä Kotlinissa, voit helposti integroida sovelluksesi muiden palveluiden kanssa.

## Katso myös

- [Kotlinin virallinen Gson-kirjasto](https://github.com/google/gson)
- [JSON-muoto ja sen käyttökohteet](https://datatracker.ietf.org/doc/html/rfc8259)
- [JSON-dokumentaatio Kotlinissa](https://kotlinlang.org/docs/working-with-json.html)