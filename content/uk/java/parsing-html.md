---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-html.md"
---

{{< edit_this_page >}}

## Що таке парсинг HTML та навіщо це потрібно?

1) Парсинг HTML - це процес розбору HTML документу на менші частини, щоб їх можна було аналізувати та обробляти. 
2) Розробники роблять це для витягування конкретних даних з веб-сторінок, зміни їх структури або навіть створення нових сторінок.

## Як це зробити:

Java бібліотека `Jsoup` чудово справляється з парсингом HTML. Ось приклад:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class Main {
    public static void main(String[] args) throws Exception {
        Document doc = Jsoup.connect("http://example.com").get();
        Elements newsHeadlines = doc.select("#news .headline");
        for (Element headline : newsHeadlines) {
            System.out.println(headline.text());
        }
    }
}
```
Цей код отримує HTML документ з `http://example.com`, знаходить всі елементи з класом "headline" в секції "news" та друкує їх текст.

## Поглиблений розгляд:

1) Історичний контекст: Парсинг HTML виник з необхідності обробки та аналізу веб-даних. Його було створено у 1990-х, коли інтернет став більш популярним. 
2) Альтернативи: Інші бібліотеки, такі як HtmlUnit та jDOM також можуть використовуватися для парсингу HTML в Java.
3) Деталі реалізації: Jsoup працює з HTML як з рядком. Він не виконує JavaScript та не підтримує AJAX.

## Див. також:

1) [Jsoup документація](https://jsoup.org/).
2) [Курс про парсинг HTML на Coursera](https://www.coursera.org/courses?query=html%20parsing).
3) [Порівняння бібліотек для парсингу HTML](https://www.slant.co/topics/6578/~best-html-parsers-for-java).