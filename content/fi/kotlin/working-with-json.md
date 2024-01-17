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

"## Mikä Ja Miksi?"

JSON on yleisesti käytetty tiedonvälitysformaatti, jota käytetään monissa ohjelmistoprojekteissa. JSON tulee sanoista JavaScript Object Notation ja se on suosittu, koska se on helppo lukea ja kirjoittaa. Käyttämällä JSONia, ohjelmoijat voivat vaihtaa tietoa eri sovellusten välillä yhteensopivalla ja yksinkertaisella tavalla.

"## Miten tehdä:"

Hyvä uutinen on, että Kotlinilla on sisäänrakennettu tuki JSON-tietojen käsittelyyn! Voit käyttää kotlinx.serialization-kirjastoa luodaksesi JSON-tietuja Kotlin-luokkien avulla ja analysoida JSON-tietoja suoraan merkkijonoina. Alla on esimerkkejä, miten voit luoda ja analysoida JSON-tietoja Kotlin-koodissa:

```Kotlin
// Luodaan JSON-tietoja Kotlin-luokkien avulla
@Serializable data class Person(val name: String, val age: Int)

// Muunnamme Kotlin-objektin JSON-muotoon
val person = Person("Matti", 30)
val json = Json.encodeToString(person)

// Analysoimme JSON-merkkijonon Kotlin-objektiksi
val parsedPerson = Json.decodeFromString(Person.serializer(), json)
println(parsedPerson.name) // tulostaa "Matti"
```

Json-kirjaston avulla voit myös käsitellä kompleksisia JSON-tietoja, kuten taulukoita ja upotettuja objekteja. Voit käyttää myös Gson-kirjastoa, jos haluat vaihtoehtoisen tavan käsitellä JSON-tietoja.

"## Syvällinen sukellus:"

JSON kehitettiin alun perin JavaScriptin yhteydessä, mutta se on nykyään suosittu formaatti kaikilla ohjelmointikielillä. JSON on myös korvannut XML-formaatin monissa sovelluksissa sen yksinkertaisuuden ja selkeyden vuoksi. Käyttämällä Kotlinia, voit helposti luoda ja analysoida JSON-tietoja ilman ylimääräistä työtä.

 "## Katso myös:"

- ["Working with JSON in Kotlin" - Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/serialization-json.html)
- ["JSON - JavaScript Object Notation" - W3Schoolsin opas JSONista](https://www.w3schools.com/js/js_json_intro.asp)
- ["Why JSON Has Replaced XML" - Blogikirjoitus JSONin ja XML:n eroista](https://www.talend.com/blog/2016/01/20/why-json-has-become-the-1-data-format-for-web-services/#:~:text=Now%2C%20JSON%20is%20the%20preferred,resource%20consumption%20with%20XML%20handling.)