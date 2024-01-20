---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego? 
Wysyłanie żądania HTTP to podstawowy sposób komunikacji między klientem a serwerem w sieci Internetowej. Programiści używają go do pobierania danych, wysyłania danych, a nawet do sterowania sprzętem podłączonym do sieci.

## Jak to zrobić:

W Kotlin to proste. Użyjemy biblioteki `ktor`. Poniżej znajduje się kod wysyłający żądanie GET.

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

val client = HttpClient()
suspend fun sendGetRequest() {
    val response: String = client.get("http://example.com")
    println(response)
}
```

W wyniku uruchomienia tego kodu zobaczysz odpowiedź serwera na żądanie GET wysłane na `http://example.com`. 

## Deep Dive

Wysyłanie żądań HTTP jest kluczowym elementem interakcji sieciowych. Historia sięga lat 90-tych, kiedy to został wprowadzony jako podstawowy protokół komunikacji w sieci WWW. 

Alternatywą dla HTTP jest HTTPS, który dodaje warstwę bezpieczeństwa przez szyfrowanie danych przesyłanych między klientem a serwerem.

Co do szczegółów implementacji, ktor, podobnie jak wiele innych bibliotek, korzysta z wzorca projektowego `builder` do tworzenia żądań HTTP. Jest to elastyczne i pozwala na dokładne sformułowanie żądania.

## Zobacz także

1. [Dokumentacja Ktor](https://ktor.io/)
2. [Protokół HTTP na Wikipedii](https://pl.wikipedia.org/wiki/HTTP)
3. [HTTPS na Wikipedii](https://pl.wikipedia.org/wiki/HTTPS)
4. [Builder Pattern na Wikipedii](https://en.wikipedia.org/wiki/Builder_pattern)