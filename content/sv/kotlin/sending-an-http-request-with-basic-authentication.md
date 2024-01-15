---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Kotlin: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Om du behöver skicka en HTTP-begäran med grundläggande autentisering, såsom en API-begäran till en webbtjänst, kan du använda Kotlin för att enkelt hantera autentiseringsprocessen.

## Hur man gör
För att skicka en HTTP-begäran med grundläggande autentisering i Kotlin, behöver du först importera nödvändiga bibliotek. Använd följande kod för att importera biblioteket "okhttp" som används för HTTP-klienter:

```Kotlin
import okhttp3.*
```

Därefter kan du initiera en HTTP-klient genom att skapa en instans av OkHttpClient-klassen:

```Kotlin
val client = OkHttpClient()
```

För att skicka en begäran med grundläggande autentisering, behöver du lägga till autentiseringshuvudet i din begäran. Du kan göra detta genom att skapa en instans av klassen "Headers" och lägga till autentiseringsuppgifter i en "Authorization" -koppling.

```Kotlin
val credentials = Credentials.basic("användarnamn", "lösenord")
val authenticationHeader = Headers.Builder().add("Authorization", credentials).build()
```

Slutligen kan du skapa en begäran med hjälp av den tidigare skapade HttpClient-instansen och lägga till autentiseringshuvudet:

```Kotlin
val begäran = Request.Builder()
    .url("https://mottagare.ns/api/endpoint")
    .header("Authorization", authenticationHeader)
    .build()
```

För att utföra begäran och få tillbaka svar, kan du använda en klientförfrågan med den skapade begäran:

```Kotlin
val svar = client.newCall(begäran).execute()
```

Om din begäran lyckades får du tillbaka ett svar med statuskod 200 (OK). Du kan sedan få tillbaka data från svaret enligt följande:

```Kotlin
val data = svar.body()?.string()
```

Hela det färdiga exemplet ser ut så här:

```Kotlin
import okhttp3.*

fun main() {
    val client = OkHttpClient()
    val credentials = Credentials.basic("användarnamn", "lösenord")
    val authenticationHeader = Headers.Builder().add("Authorization", credentials).build()
    
    val begäran = Request.Builder()
        .url("https://mottagare.ns/api/endpoint")
        .header("Authorization", authenticationHeader)
        .build()
    
    val svar = client.newCall(begäran).execute()
    
    if (svar.isSuccessful) {
        val data = svar.body()?.string()
        println("Svar:\n$data")
    } else {
        println("Fel: " + svar.code() + "-" + svar.message())
    }
}
```

Sample output: 
Svar:
{"id": 1234, "namn": "John Doe"}

## Djupdykning
Att skicka en HTTP-begäran med grundläggande autentisering innebär att inkludera autentiseringsinformation i HTTP-headers. Detta gör det möjligt för tjänsten att verifiera identiteten hos den som skickar begäran utan att behöva dela känslig data som användaruppgifter.

Det är viktigt att notera att grundläggande autentisering inte är den mest säkra formen av autentisering eftersom användaruppgifterna skickas i klartext. Det är därför viktigt att bara använda den för att hämta data som inte är känslig.

## Se även
- Officiell Kotlin Dokumentation: https://kotlinlang.org/
- Guide för att skicka HTTP-begäran i Kotlin: https://www.programiz.com/kotlin-programming/http-client
- OkHttp bibliotek: https://square.github.io/okhttp/