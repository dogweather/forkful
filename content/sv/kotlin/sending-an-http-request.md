---
title:                "Att skicka en http-begäran"
html_title:           "Kotlin: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan innebär att be en server om information via internet. Programmerare gör detta för att hämta data från en annan plats, till exempel från en API.

## Hur man:
```Kotlin
val url = "https://example.com"
val client = OkHttpClient()

val request = Request.Builder()
    .url(url)
    .build()

try {
    val response = client.newCall(request).execute()
    val body = response.body?.string()
    println(body)
} catch (e: IOException) {
    e.printStackTrace()
}
```

Kodexemplet visar hur man skickar en enkel HTTP-förfrågan i Kotlin med hjälp av OkHttp-biblioteket. Först sätts en URL till önskad plats och en HTTP-klient skapas. Sedan byggs en begäran med hjälp av URL:en och tillslut utförs förfrågan och responsen skrivs ut till konsolen.

## Deep Dive
Att skicka HTTP-förfrågningar har funnits sedan starten av internet och är en fundamental del av webbaserade applikationer. Det finns också andra bibliotek och ramverk som kan användas för att skicka HTTP-förfrågningar, som till exempel Retrofit och Ktor. 

För att skicka en HTTP-förfrågan behöver vi en URL för målplatsen och en metod som bestämmer vad som ska göras med denna URL. I vårt fall är det en GET-förfrågan som används, vilket betyder att vi vill hämta data från den angivna URL:en. För att utföra förfrågan behöver vi också en HTTP-klient som hanterar kommunikationen med servern. 

## Se också
- [OkHttp](https://square.github.io/okhttp/) - Officiell webbplats för OkHttp-biblioteket
- [Retrofit](https://square.github.io/retrofit/) - Ett annat populärt bibliotek för att skicka HTTP-förfrågningar i Kotlin
- [Ktor](https://ktor.io/) - Ett ramverk för att bygga webbapplikationer i Kotlin som också har inbyggda funktioner för att skicka HTTP-förfrågningar.