---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:34.910390-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Kotlin \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\
  \u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0434\u0435\u043B\u0430\u0435\u0442\u0441\
  \u044F \u043F\u0440\u043E\u0441\u0442\u043E. \u0412\u043E\u0442 \u0431\u0430\u0437\
  \u043E\u0432\u044B\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C `khttp`,\
  \ \u0434\u0440\u0443\u0436\u0435\u0441\u0442\u0432\u0435\u043D\u043D\u043E\u0439\
  \ \u043A \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438."
lastmod: '2024-03-13T22:44:44.972548-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\
  \u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0434\u0435\u043B\u0430\u0435\u0442\u0441\
  \u044F \u043F\u0440\u043E\u0441\u0442\u043E."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
В Kotlin отправка HTTP-запросов делается просто. Вот базовый пример с использованием `khttp`, дружественной к пользователю библиотеки:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

Вывод:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

Для более сложных задач вот фрагмент с использованием `ktor`, фреймворка Kotlin, для асинхронной загрузки данных:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: String = client.get("https://api.github.com/users/octocat/orgs")
    println(response)
    client.close()
}
```

Вывод аналогичен первому примеру.

## Глубокое Погружение
Библиотека `khttp` является удобным инструментом, созданным по образцу `requests` из Python. Она отлично подходит для быстрых скриптов, но не обновлялась в течение некоторого времени. `ktor` – это более новый активный проект от JetBrains, разработанный с использованием корутин для асинхронных операций. Он предназначен для масштабируемых приложений. Обе библиотеки обрабатывают HTTP-запросы, но предназначены для разных случаев использования.

Исторически HTTP-запросы в Kotlin выполнялись с помощью Java-библиотек, таких как `HttpURLConnection` или `HttpClient` от Apache. Эти методы все еще актуальны, но они более громоздкие и не имеют преимуществ языковых особенностей Kotlin.

Что касается реализации, помните о необходимости обработки общих ошибок HTTP и чтения кода ответа. Также вам понадобится использовать `try-catch` для сетевых исключений и возможно работать с заголовками и параметрами запроса.

## Смотрите Также
- Документация Ktor: https://ktor.io/
- Репозиторий khttp на GitHub: https://github.com/jkcclemens/khttp (Обратите внимание на статус поддержки)
- Выполнение HTTP-запросов в Kotlin с использованием HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/
