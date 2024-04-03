---
date: 2024-01-20 17:44:16.039786-07:00
description: "How to: Kotlin pozwala na proste pobieranie tre\u015Bci sieciowej. Oto\
  \ podstawowy przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.363775-06:00'
model: gpt-4-1106-preview
summary: "Kotlin pozwala na proste pobieranie tre\u015Bci sieciowej."
title: Pobieranie strony internetowej
weight: 42
---

## How to:
Kotlin pozwala na proste pobieranie treści sieciowej. Oto podstawowy przykład:

```Kotlin
import java.net.URL

fun main() {
    val websiteContent = URL("http://example.com").readText()
    println(websiteContent)
}
```

Wyjście (sample output):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```
Ten kod wyświetli kod HTML strony example.com.

## Deep Dive
Pobieranie stron internetowych jest starym jak WWW. Historia sięga protokołu HTTP i HTML. Alternatywami są biblioteki jak OkHttp lub Retrofit dla funkcji bardziej zaawansowanych, takich jak obsługa błędów i asynchroniczność.

```Kotlin
import okhttp3.OkHttpClient
import okhttp3.Request

fun main() {
    val client = OkHttpClient()
    val request = Request.Builder()
                     .url("http://example.com")
                     .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Błąd: ${response.code}")

        println(response.body?.string())
    }
}
```

Korzystanie z zewnętrznych bibliotek daje większą kontrolę i bezpieczeństwo. Pamiętaj o dodaniu zależności w Gradle.

## See Also
- OkHttp: https://square.github.io/okhttp/
- Retrofit: https://square.github.io/retrofit/
- Dokumentacja Kotlin: https://kotlinlang.org/docs/reference/
- Przewodnik po HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
