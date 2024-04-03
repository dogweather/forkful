---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:43.879031-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C `HttpURLConnection` Kotlin \u0434\
  \u043B\u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E \u0437\u0430\u0445\
  \u0432\u0430\u0442\u0430 \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\u043D\u0438\
  \u0446\u044B. \u041C\u044B \u0442\u0430\u043A\u0436\u0435 \u0431\u0443\u0434\u0435\
  \u043C \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u043A\u043E\u0440\u0443\u0442\u0438\u043D\u044B \u0434\u043B\u044F \u043F\u043B\
  \u0430\u0432\u043D\u043E\u0439\u2026"
lastmod: '2024-03-13T22:44:44.976030-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C `HttpURLConnection` Kotlin \u0434\u043B\
  \u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E \u0437\u0430\u0445\u0432\
  \u0430\u0442\u0430 \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\
  \u044B."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Давайте использовать `HttpURLConnection` Kotlin для быстрого захвата веб-страницы. Мы также будем использовать корутины для плавной работы в фоновом режиме. Вот краткое руководство:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.*

fun main() = runBlocking {
    val url = "http://example.com"
    val result = withContext(Dispatchers.IO) {
        downloadWebPage(url)
    }
    println(result)
}

fun downloadWebPage(urlAddress: String): String {
    val url = URL(urlAddress)
    val connection = url.openConnection() as HttpURLConnection
    try {
        connection.connect()
        return connection.inputStream.bufferedReader().use { it.readText() }
    } finally {
        connection.disconnect()
    }
}
```

Пример вывода:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
Неплохо, правда? Вы получили HTML веб-страницы!

## Глубокое погружение
Загрузка веб-страниц настолько же стара, как и сам веб. В 90-х люди использовали инструменты командной строки, такие как `wget` и `curl`. Они всё ещё используются, но когда вам нужен больший контроль или нужно интегрировать получение веб-контента в приложение, вы пишете код.

В Kotlin вы могли бы использовать `HttpURLConnection` Java или библиотеки, такие как OkHttp или Ktor, для более мощного подхода с дополнительными функциями. Приведенный выше пример базовый; в реальной жизни вы подумали бы об обработке ошибок, переадресациях и производительности. Может быть, добавить повторные попытки или тайм-аут? И не забывайте об обработке различных кодировок символов и типов контента.

Вы также подумали бы о потоках. Не хотели бы мы вешать главный поток при загрузке огромной страницы, не так ли? Поэтому корутины - они позволяют вашему приложению оставаться отзывчивым, загружая контент в фоновом режиме без перегружения.

## Смотрите также
- **OkHttp**: https://square.github.io/okhttp/
- **Ktor Client**: https://ktor.io/docs/client.html
- **Корутины Kotlin**: https://kotlinlang.org/docs/coroutines-overview.html
- **Java HttpURLConnection**: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html

Вот и всё — получите страницу, будьте умны в деталях и всегда уважайте данные и их источник. Счастливого кодинга!
