---
title:                "Аналізування html"
html_title:           "Kotlin: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Чому: 

Документи HTML є основою для будь-якого веб-сайту та містять значну кількість інформації. Парсинг HTML дозволяє легко витягнути цю інформацію та використовувати її для подальшої обробки або аналізу.

## Як:

Парсинг HTML є простим за допомогою мови програмування Kotlin. Нижче наведено код та вихідні дані в "```Kotlin ... ```" блоках коду.

Припустимо, що ви маєте такий HTML документ:

```Kotlin 
<html>
    <head>
        <title>Текст сторінки</title>
    </head>
    <body>
        <div>
            <h1>Привіт, світ!</h1>
            <p>Це текст на сторінці.</p>
            <p>Це другий абзац.</p>
        </div>
    </body>
</html>
```

Щоб витягнути текст з елементів `<p>`, виконайте наступний код:

```Kotlin 
val doc = Jsoup.parse(htmlString)
val paragraphs = doc.select("p") // вибирати всі елементи <p>
for (p in paragraphs) {
    println(p.text()) // вивести текст кожного елементу <p>
}
```

Результат виводу буде наступним:

```
Це текст на сторінці.
Це другий абзац.
```

## Заглиблення:

У мові Kotlin є спеціальна бібліотека для парсингу HTML - Jsoup. Вона дозволяє з легкістю отримувати доступ до елементів та атрибутів документу HTML за допомогою CSS селекторів. Також можна використовувати регулярні вирази для пошуку певних елементів або шаблонів.

Додатково, Jsoup має можливість парсити сайти за допомогою URL-адрес. Це дозволяє зручно отримувати інформацію зі сторінок веб-сайтів.

## Дивись також:

- [Офіційна документація по парсингу HTML за допомогою Jsoup в мові Kotlin](https://jsoup.org/cookbook/extracting-data/selector-syntax)
- [Стаття про парсинг HTML за допомогою Kotlin та Jsoup](https://medium.com/@arconsultur/how-to-use-kotlin-and-jsoup-to-write-a-simple-web-scraper-for-parsing-html-bc1f46f9e779)