---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att vi använder en bestämd användarnamn och lösenord-kombination för att begära data från servern. Programmerare gör det för att skydda känslig data och hålla webbtjänster säkra.

## Hur man gör:
Först, lägg till `Ktor-client-auth` till ditt beroenden. Sedan, du kan enkelt skicka en HTTP-begäran i Kotlin. Se nedan:
```Kotlin
val client = HttpClient() {
    install(Auth) {
        basic {
            username = "username"
            password = "password"
        }
    }
}

runBlocking {
    val response: HttpResponse = client.get("https://www.example.com")

    println(response.readText())
}
```
Detta kodblock skapar en HTTP-klient, installerar Basic Auth med användarnamn och lösenord, och sedan gör en GET-förfrågan till `www.example.com`. Svaret skrivs sedan ut i konsolen.

## Djup dykning:
Grundläggande autentisering har bestått en lång tid och det är en traditionell metod för att skydda webbtjänster. Men det finns andra alternativ, som OAuth och JWT, som ger mer robusta och flexibla lösningar. När det gäller att implementera HTTP-begäran med basic autentisering i Kotlin, 'Ktor' och 'OkHttp' är populära bibliotek på grund av deras enkelhet och effektivitet.

## Se även:
- Ktor Basic Authentication: [https://ktor.io/docs/basic.html](https://ktor.io/docs/basic.html)
- OkHttp Basic Authentication: [https://square.github.io/okhttp/recipes/#basic-authentication-kt-java](https://square.github.io/okhttp/recipes/#basic-authentication-kt-java)
- Alternativ för autentisering: [https://oauth.net/](https://oauth.net/) och [https://jwt.io/introduction/](https://jwt.io/introduction/)