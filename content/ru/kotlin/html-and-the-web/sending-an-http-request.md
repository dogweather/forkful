---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:34.910390-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса похожа на просьбу веб-серверу что-то сделать или что-то дать. Программисты делают это для взаимодействия с веб-сервисами, получения данных, отправки форм или общения с API.

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
