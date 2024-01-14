---
title:                "Kotlin: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Kehittäjillä on usein tarve lähettää HTTP-pyyntöjä ulkoisille palveluille tai sovelluksille. Tämä voi olla tarpeen esimerkiksi tietojen hakemiseen, tallentamiseen tai päivittämiseen.

## Miten

Kotlinilla on helppo lähettää HTTP-pyyntöjä käyttämällä sisäänrakennettua `URL`-luokkaa ja `HttpURLConnection`-oliota. Seuraavassa esimerkissä käytämme `GET`-pyyntöä ja tulostamme vastauksen sisällön:

```Kotlin
val url = URL("https://api.example.com/users")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
val responseCode = connection.responseCode
if (responseCode == HttpURLConnection.HTTP_OK) {
    val content = connection.inputStream.bufferedReader().use { it.readText() }
    println(content)
}
```

**Output:**

```
[{"id": 123, "name": "John"}, {"id": 456, "name": "Jane"}]
```

Voit myös lähettää muita HTTP-metodeja, kuten `POST`, `PUT` ja `DELETE`, käyttämällä `setRequestMethod()` -metodia.

## Syväsukellus

HTTP-pyyntö koostuu yleensä seuraavista osista:

- **Request Line:** sisältää HTTP-metodin, URL-osoitteen ja protokollaversion.
- **Headerit:** sisältää lisätietoja pyynnöstä, kuten käyttäjäagentin ja sisällön tyyppi.
- **Body:** sisältää tarvittaessa tietoja tai sisällön, esimerkiksi `POST`-pyynnössä.

Käyttämällä `setRequestMethod()` ja `setRequestProperty()` -metodeja voit määrittää nämä osat haluamallasi tavalla.

## Katso myös

- [Kotlinin `URL`-luokan dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-u-r-l/)
- [Java: Lähetä HTTP-pyyntö ja saa vastaus](https://www.baeldung.com/java-http-request)
- [HTTP Request & Response Tutorial](https://www.tutorialspoint.com/http/http_requests.htm)