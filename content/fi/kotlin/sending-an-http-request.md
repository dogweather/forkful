---
title:                "Lähettämällä http-pyyntö"
html_title:           "Kotlin: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi

HTTP-pyyntöjen lähettäminen on olennainen osa modernia ohjelmointia, ja niitä käytetään esimerkiksi web-sovellusten ja palveluiden kommunikointiin. Kotlinin avulla voit helposti lähettää ja käsitellä HTTP-pyyntöjä yksinkertaisesti ja tehokkaasti.

# Kuinka

```Kotlin
// Luo uusi HTTP-yhteys
val url = URL("https://www.example.com")
val connection = url.openConnection() as HttpURLConnection

// Määritä pyynnön tyyppi ja aseta tarvittavat ominaisuudet
connection.requestMethod = "GET"
connection.setRequestProperty("Content-Type", "application/json")
connection.doOutput = true

// Lähetä pyyntö ja lue vastaus
val responseCode = connection.responseCode
val responseData = connection.inputStream.bufferedReader().readText()

// Tulosta vastauskoodi ja data
println("Response code: $responseCode")
println("Response data: $responseData")
```

Tämä on yksinkertainen esimerkki GET-pyynnön lähettämisestä ja vastauksen käsittelystä. Voit muuttaa pyynnön tyyppiä ja lisätä tai muokata pyynnön ominaisuuksia tarpeen mukaan.

# Syväsukellus

HTTP-pyyntöjen lähettäminen Kotlinilla perustuu Java:n java.net-pakettiin. Tämän avulla voimme luoda yhteyden haluttuun URL-osoitteeseen ja asettaa pyynnön parametrit. Voit myös lisätä esimerkiksi autentikointitiedot tai muokata pyynnön dataa ennen sen lähettämistä.

Pyynnön lähettämisen jälkeen voimme tarkistaa vastauksen koodin ja lukea vastauksen datan haluamallamme tavalla. Kotlinin laaja valikoima standardikirjastoja tekee HTTP-pyyntöjen käsittelystä helppoa ja tehokasta.

# Katso myös

- Java.net-paketti: https://docs.oracle.com/javase/8/docs/api/java/net/package-summary.html
- Kotlinin standardikirjasto: https://kotlinlang.org/api/latest/jvm/stdlib/