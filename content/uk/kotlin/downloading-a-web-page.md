---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Завантаження веб-сторінки - це процес отримання інформації з інтернету. Програмісти це роблять, щоб аналізувати, обробляти її або використовувати на своїх власних веб-сайтах.

## Як це робиться:

```Kotlin
import java.net.URL

fun main() {
    val url = URL("http://example.com")
    val content = url.readText()
    println(content)
}
```

Вищенаведений код Kotlin створює `URL` об'єкт для `http://example.com` , очитує його текст та виводить його на консоль.

## Поглиблений погляд:

Завантаження веб-сторінки не є новою концепцією. Ми робимо це щоразу, коли переходимо на веб-сайт. Проте, на відміну від користувача, що просто переглядає веб-сторінку, програмісти можуть завантажувати її, щоб змінити її структуру або отримати цінну інформацію. І хоча ми використали Java `URL` в цьому прикладі, вам можуть бути знайомі інші засоби, як-от `java.net.http.HttpClient`.

Якщо хочете розширити цей приклад, можете використати бібліотеку Jsoup, що надає додаткові інструменти для роботи з HTML.

## Дивіться також:

Для додаткової інформації та різних варіантів реалізації завантаження веб-сторінок в Kotlin можна звернутися до наступних джерел:
- Офіційна документація Kotlin: https://kotlinlang.org/docs/tutorials/kotlin-for-py/networking.html
- Використання HttpClient в Kotlin: https://www.baeldung.com/kotlin-http-request
- Jsoup документація: https://jsoup.org/