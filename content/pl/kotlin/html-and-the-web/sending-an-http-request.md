---
date: 2024-01-20 17:59:55.549167-07:00
description: "How to: (Jak to zrobi\u0107:) W Kotlinie najlepiej u\u017Cy\u0107 biblioteki\
  \ `khttp` do prostych \u017C\u0105da\u0144 HTTP. Dajmy na to tak."
lastmod: '2024-03-13T22:44:35.361809-06:00'
model: gpt-4-1106-preview
summary: "W Kotlinie najlepiej u\u017Cy\u0107 biblioteki `khttp` do prostych \u017C\
  \u0105da\u0144 HTTP."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
