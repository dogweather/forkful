---
date: 2024-01-20 18:02:24.949096-07:00
description: "Skicka en HTTP-f\xF6rfr\xE5gan med basic-autentisering inneb\xE4r att\
  \ koda ihop anv\xE4ndarnamn och l\xF6senord och skicka med det i en f\xF6rfr\xE5\
  gans `Authorization`\u2026"
lastmod: '2024-03-13T22:44:37.870927-06:00'
model: gpt-4-1106-preview
summary: "Skicka en HTTP-f\xF6rfr\xE5gan med basic-autentisering inneb\xE4r att koda\
  \ ihop anv\xE4ndarnamn och l\xF6senord och skicka med det i en f\xF6rfr\xE5gans\
  \ `Authorization`\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
---

{{< edit_this_page >}}

## Vad & Varför?
Skicka en HTTP-förfrågan med basic-autentisering innebär att koda ihop användarnamn och lösenord och skicka med det i en förfrågans `Authorization` header. Vi gör det för att säkerställa att endast behöriga användare får tillgång till skyddade resurser.

## Så här gör man:
Här är ett exempel på hur man skickar en HTTP-förfrågan med basic-autentisering i Kotlin med hjälp av `java.net.HttpURLConnection`:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val authHeaderValue = "Basic " + Base64.getEncoder().encodeToString(credentials.toByteArray())

    connection.requestMethod = "GET"
    connection.setRequestProperty("Authorization", authHeaderValue)

    val responseCode = connection.responseCode
    println("Response Code: $responseCode")

    // Läs svaret om förfrågan lyckades
    if (responseCode == HttpURLConnection.HTTP_OK) {
        connection.inputStream.bufferedReader().use { reader ->
            println("Response: ${reader.readText()}")
        }
    } else {
        println("Failed to authenticate.")
    }
}

fun main() {
    val testUrl = "https://example.com/protected"
    val username = "user"
    val password = "password"
    sendGetRequestWithBasicAuth(testUrl, username, password)
}
```
Output:
```
Response Code: 200
Response: {response from the server}
```
Eller så använder man ett bibliotek som `OkHttp` för att göra processen smidigare:
```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request

fun sendGetRequestWithOkHttp(url: String, username: String, password: String) {
    val client = OkHttpClient()
    val credentials = Credentials.basic(username, password)
    val request = Request.Builder()
        .url(url)
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        println("Response Code: ${response.code}")
        println("Response: ${response.body?.string()}")
    }
}

fun main() {
    val testUrl = "https://example.com/protected"
    val username = "user"
    val password = "pass"
    sendGetRequestWithOkHttp(testUrl, username, password)
}
```
Output:
```
Response Code: 200
Response: {response from the server}
```

## Fördjupning
Basic-autentisering finns länge och fungerar genom att man använder `Base64` för att koda användaruppgifter. Att det är enkelt är bra, men säkerheten är inte toppen då `Base64` inte är kryptering. HTTPS är ett måste när detta används.

Moderna alternativ till basic-autentisering inkluderar OAuth och API-nycklar. De erbjuder bättre säkerhet och flexibilitet, men kan vara mer komplext att implementera och förvalta.

När man implementerar basic-autentisering är det viktigt att hantera lösenord korrekt. Lagra aldrig okrypterade lösenord och kom ihåg att autentiseringsuppgifterna alltid ska skickas över en säker anslutning.

## Se även
- [OkHttp's GitHub repo](https://github.com/square/okhttp)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Basic access authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Kotlin Programming Language](https://kotlinlang.org/)
