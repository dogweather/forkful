---
title:                "Парсинг html"
html_title:           "Java: Парсинг html"
simple_title:         "Парсинг html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-html.md"
---

{{< edit_this_page >}}

## Що & Чому?
Разбирання HTML це процес аналізування інформації, що міститься на веб-сторінках. Програмісти використовують парсинг HTML для отримання потрібної інформації з веб-сторінок, такої як заголовки, текст або посилання.

## Як це зробити:
```Java
Document doc = Jsoup.connect("https://www.example.com/").get();
String title = doc.title();
Element body = doc.body();
Elements links = body.select("a[href]");

System.out.println("Заголовок сторінки: " + title);
System.out.println("Текст сторінки: " + body.text());

for (Element link : links) {
    System.out.println("Посилання: " + link.attr("href"));
}
```

Вивід:

```
Заголовок сторінки: Приклад
Текст сторінки: Це прикладна веб-сторінка.
Посилання: https://www.example.com/about
Посилання: https://www.example.com/contact
```

## Докладно про це:
Розбирання HTML з'явилося в 1990-х роках, коли створювалася перша версія веб-браузера. Існують також інші методи отримання даних з веб-сторінок, наприклад, використання API або скрапінгу. Однак парсинг HTML є простим і ефективним способом для отримання потрібної інформації.

## Дивіться також:
- [Інформація про Jsoup у документації Java](https://jsoup.org/)
- [Зразки реального парсингу HTML за допомогою Java](https://www.baeldung.com/java-html-parsing-jsoup) 
- [Документація зі сніпетами Jsoup](https://jsoup.org/cookbook/extracting-data/selector-syntax)