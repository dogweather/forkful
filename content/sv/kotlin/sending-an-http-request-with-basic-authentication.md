---
title:                "Kotlin: Skicka en http-begäran med grundautentisering"
simple_title:         "Skicka en http-begäran med grundautentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle någon vilja skicka en HTTP-begäran med grundläggande autentisering? Det finns flera olika scenarier där en sådan begäran kan vara användbar. Till exempel, om du vill ansluta till en webbtjänst som kräver autentisering, eller om du vill få tillgång till skyddade resurser på en webbplats.

## Så här gör du

För att skicka en HTTP-begäran med grundläggande autentisering i Kotlin, följ följande steg:

1. Importera nödvändiga klasser:

```Kotlin
import java.net.URL
import java.net.HttpURLConnection
import java.util.Base64
```

2. Skapa en URL för den resurs du vill komma åt:

```Kotlin
val url = URL("https://www.example.com/api/resource")
```

3. Skapa en HttpURLConnection och öppna anslutningen:

```Kotlin
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
```

4. Konstruera en autentiseringssträng genom att kombinera användarnamn och lösenord och kodera dem i Base64-format:

```Kotlin
val username = "användarnamn"
val password = "lösenord"
val authString = "$username:$password"
val authStringEnc = Base64.getEncoder().encodeToString(authString.toByteArray())
```

5. Lägg till autentiseringshuvudet till begäran:

```Kotlin
connection.setRequestProperty("Authorization", "Basic $authStringEnc")
```

6. Skicka begäran och hämta svarskoden:

```Kotlin
val responseCode = connection.responseCode
```

7. Om svarskoden är 200, betyder det att begäran gick igenom och du kan hämta innehållet i svarskroppen som en sträng:

```Kotlin
if (responseCode == 200) {
    val responseBody = connection.inputStream.bufferedReader().use { it.readText() }
    println(responseBody)
}
```

## Djupdykning

När du skickar en HTTP-begäran med grundläggande autentisering, lägger du helt enkelt till ett autentiseringshuvud i begäran som innehåller användarnamn och lösenord i krypterad form. Detta gör det möjligt för servern att verifiera identiteten för begäran och ge tillgång till skyddad information eller tjänster. Det är viktigt att notera att om du använder denna metod för autentisering, bör du kryptera anslutningen med HTTPS för att skydda dina användarnamn och lösenord från eventuella angripare.

## Se även

- [Java Base64 klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [HTTP-begäran](https://docs.oracle.com/javase/tutorial/networking/urls/request.html)
- [HTTP-begäran med Java](https://www.baeldung.com/java-http-request)