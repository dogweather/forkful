---
title:                "Sända en http-förfrågan"
html_title:           "Kotlin: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Du undrar kanske varför du bör lära dig skicka HTTP-förfrågningar i Kotlin. Det kan vara en användbar färdighet för att interagera med API:er och hämta data från webbplatser.

## Så här gör du det

För att skicka en HTTP-förfrågan i Kotlin, behöver du först importera följande paket:

```Kotlin
import java.net.HttpURLConnection
```

Sedan behöver du skapa en instans av HttpURLConnection-klassen och ange vilken URL du vill skicka förfrågan till. Du kan använda `openConnection()`-metoden för att skapa en anslutning till en URL:

```Kotlin
val url = URL("https://example.com/api")
val connection = url.openConnection() as HttpURLConnection
```

Nu är det dags att välja vilken metod du vill använda för att skicka din förfrågan, till exempel `GET`, `POST` eller `PUT`. Du kan göra det genom att ange metoden i anslutningens `requestMethod`-egenskap:

```Kotlin
connection.requestMethod = "GET"
```

Om du skickar en `POST`-förfrågan, måste du även ange innehållet som du vill skicka med i förfrågan genom att ange lämplig ström för anslutningen:

```Kotlin
val postData = "name=John&age=30"
connection.outputStream.use { outputStream ->
    outputStream.write(postData.toByteArray(Charsets.UTF_8))
}
```

Slutligen, för att faktiskt skicka förfrågan, måste du anropa `connect()`-metoden och sedan få svarskod och data från ansökan:

```Kotlin
connection.connect()
val responseCode = connection.responseCode
val responseMessage = connection.responseMessage
```

## Djupdykning

Det finns mer att utforska när det gäller att skicka HTTP-förfrågningar i Kotlin. Du kan till exempel använda olika metoder för att återuppta avbrutna förfrågningar, hantera autentisering eller hantera cookies. Du kan också använda bibliotek som OkHttp eller Retrofit för att förenkla förfrågningarna.

## Se även

- [Kotlin Standardbibliotek](https://kotlinlang.org/api/latest/jvm/stdlib/index.html)
- [Dokumentation för HttpURLConnection](https://developer.android.com/reference/java/net/HttpURLConnection)