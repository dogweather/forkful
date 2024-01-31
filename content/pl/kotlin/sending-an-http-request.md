---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T17:59:55.549167-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wysyłanie żądania HTTP to proces komunikacji z serwerem – pytasz, serwer odpowiada. Programiści robią to, aby pobierać dane, wysyłać informacje lub korzystać z usług sieciowych.

## How to: (Jak to zrobić:)
W Kotlinie najlepiej użyć biblioteki `khttp` do prostych żądań HTTP. Dajmy na to tak:

```Kotlin
import khttp.get

fun retrieveWebpage() {
    val response = get("https://example.com")
    println(response.text)
}

fun main() {
    retrieveWebpage()
}
```

Jeśli to uruchomisz, powinieneś zobaczyć zawartość strony example.com w konsoli.

## Deep Dive (W pogłębieniu)
Zanim pojawiły się interfejsy API ułatwiające wysyłanie HTTP, programiści musieli pracować bezpośrednio z gniazdami TCP/IP, co było bardziej skomplikowane. Dziś, poza `khttp`, możliwości to `HttpURLConnection`, biblioteka OkHttp, czy używanie bibliotek asynchronicznych jak Ktor dla aplikacji wielowątkowych. Wybór zależy od potrzeb: `khttp` jest proste i synchroniczne, `OkHttp` bardziej wydajne, a `Ktor` idealne do większych, skalowalnych systemów.

## See Also (Zobacz także)
- Oficjalna dokumentacja `khttp`: http://khttp.readthedocs.io
- OkHttp: https://square.github.io/okhttp/
- Ktor: https://ktor.io/clients/http-client.html
- Dokumentacja Kotlin: https://kotlinlang.org/docs/home.html

Pamiętaj, że linki prowadzą do dokumentacji w języku angielskim.
