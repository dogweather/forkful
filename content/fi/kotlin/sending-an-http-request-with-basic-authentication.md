---
title:                "Kotlin: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Miksi lähettää HTTP-pyyntö perusautentikoinnilla?

HTTP-pyyntöjen lähettäminen perusautentikoinnin avulla mahdollistaa turvallisen tavan lähettää ja vastaanottaa tietoa verkkosovellusten välillä. Perusautentikointi vaatii tunnistautumisen käyttäjän nimen ja salasanan avulla, jolloin tiedonsiirto pysyy luottamuksellisena.

## Kuinka tehdä se

```Kotlin
val url = URL("https://example.com/api/endpoint") // Määritetään URL, johon pyyntö lähetetään
val conn = url.openConnection() as HttpURLConnection // Avataan yhteys URL-osoitteeseen
conn.requestMethod = "GET" // Pyyntömetodi, tässä esimerkissä GET
val userCredentials = "käyttäjänimi:salasana".toByteArray() // Muutetaan käyttäjänimi:salasana -pari tavuiksi
val basicAuth = "Basic " + Base64.getEncoder().encodeToString(userCredentials) // Luodaan Authorize-header
conn.setRequestProperty ("Authorization", basicAuth) // Lisätään Authorize-header pyyntöön
val responseCode = conn.responseCode // Vastauksen statuskoodi
if (responseCode == HttpURLConnection.HTTP_OK) { // Tarkistetaan onko pyyntö onnistunut
    println("Pyyntö onnistui")
    val input = BufferedReader(InputStreamReader(conn.inputStream)) // Vastaanotetun datan lukeminen
    var inputLine: String?
    val response = StringBuffer()
    while (input.readLine().also { inputLine = it } != null) { // Luetaan data rivi kerrallaan
        response.append(inputLine)
    }
    input.close() // Suljetaan datan lukija
    println(response.toString()) // Tulostetaan vastaanotettu data
} else {
    println("Pyyntö epäonnistui") // Tulostetaan virheilmoitus, jos pyyntö epäonnistui
}
```

Esimerkiksi, jos käyttäjän käyttäjänimi on "käyttäjä1" ja salasana "salasana1" ja pyyntö onnistuu, vastauksena saadaan:

```Kotlin
Pyyntö onnistui 
{"message":"Tervetuloa, käyttäjä1!"} // Vastaanotettu JSON-data
```

## Syvempää tietoa

Perusautentikointi toteutetaan HTTP-pyynnöissä Header-välilehdellä, erityisesti Authorize-headerilla. Se koostuu käyttäjänimen ja salasanan yhdistelmästä ("käyttäjänimi:salasana"), joka muutetaan Base64-muotoon ja lisätään "Basic" -sanalla eteen. Tämä takaa, että käyttäjänimi ja salasana eivät ole suoraan luettavissa.

## Katso myös

- [Kotlin-tutoriaali HTTP-pyyntöjen lähettämisestä](https://kotlinlang.org/docs/networking.html)
- [HTTP-autentikointi Wikipediassa](https://fi.wikipedia.org/wiki/HTTP-autentikointi)
- [Base64-koodaus Wikipediassa](https://fi.wikipedia.org/wiki/Base64)