---
title:                "Työskentely jsonin kanssa"
html_title:           "Kotlin: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# Miksi: Miksi JSON:in kanssa kannattaa työskennellä?

Koska JSON on yksi yleisimmistä tiedonsiirtomuodoista, joka mahdollistaa tiedon siirtämisen ja tallentamisen eri sovellusten välillä. Sen yksinkertainen syntaksi ja helppo luettavuus tekevät siitä suositun vaihtoehdon monille ohjelmoijille. 

## Kuinka: Esimerkkejä koodista ja tulostuksesta

Kotlinilla on valmiita työkaluja JSON:in käsittelyyn, mikä tekee siitä helpon ja vaivattoman työkalun työskennellä JSON:in kanssa. Alla on esimerkkejä siitä, kuinka voit lukea ja kirjoittaa JSON:ia Kotlinilla.

```Kotlin
// Luodaan JSON objekti käyttäen Kotlinin HashMapia
val user = hashMapOf(
    "name" to "Matti",
    "age" to 25
)

// Muunnetaan HashMap JSON:iksi
val jsonUser = JSONObject(user)

// Tulostetaan JSON
println(jsonUser)

```

Tässä esimerkissä luodaan Kotlinin HashMap, johon lisätään kaksi avainta ja arvoa. Sitten muunnetaan tämä HashMap JSON-muotoon käyttäen JSONObject-luokkaa ja lopuksi tulostetaan JSON. Tulostus näyttää seuraavalta:

```
{ "name": "Matti", "age": 25 }
```

```Kotlin
// Luetaan JSON:ia tiedostosta
val jsonString = File("users.json").readText()

// Muunnetaan JSON Kotlinin HashMapiksi
val usersMap = JSON.parseObject(jsonString, object : TypeReference<HashMap<String, Any>>(){})

// Tulostetaan kaikki käyttäjät
for ((name, age) in usersMap) {
    println("Name: $name, Age: $age")
}

```

Toisessa esimerkissä luetaan JSON-tiedosto ja muunnetaan se HashMapiksi käyttäen Kotlinin JSON-kirjastoa. Sitten tulostetaan kaikki käyttäjät käyttäen for-silmukkaa. Tulostus näyttää tältä:

```
Name: Matti, Age: 25
Name: Liisa, Age: 30
```

## Syvemmälle: Lisää tietoa JSON:in käsittelystä

JSON koostuu avaimista ja arvoista, jotka ovat tallennettuina tekstiin. Avaimet ja arvot erotetaan kaksoispisteellä ja parit pilkulla. JSON tukee myös erilaisia tietotyyppejä, kuten numeroita, merkkijonoja, totuusarvoja ja listoja.

JSON:in suosio johtuu sen yksinkertaisesta syntaksista ja se sopii hyvin siirtämiseen internetin kautta. Suurin osa modernista web-sovelluskehityksestä käyttää JSON:ia, joten sen hyväksikäyttö on erittäin hyödyllistä.

# Katso myös

- [Kotlinin virallinen JSON-kirjasto: Gson](https://github.com/google/gson)
- [Esimerkkejä JSON:in käsittelystä Kotlinilla](https://www.baeldung.com/kotlin-json)