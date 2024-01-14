---
title:                "Java: Розбір html"
simple_title:         "Розбір html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Перехід до парсингу HTML може бути корисним для програмістів у різних сферах роботи. Він допомагає витягувати цінну інформацію з HTML сторінок, що може бути використана для створення веб-скраперів, веб-кравлерів або інших автоматизаційних інструментів.

## Як

Для початку роботи з парсером HTML в Java потрібно встановити бібліотеку Jsoup. Для цього можна скористатися Maven або Gradle залежно від вашого проекту. Після цього імпортуємо бібліотеку у файл коду.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

public class ParsingExample {
    public static void main(String[] args) {
        // Завантажуємо HTML сторінку з URL адресою
        Document doc = Jsoup.connect("https://www.example.com").get();
        
        // Витягуємо усі елементи з тегом "a"
        Elements links = doc.select("a");
        
        // Виводимо текст усіх знайдених посилань
        for (Element link : links) {
            System.out.println(link.text());
        }
    }
}
```

В результаті виконання цього коду, ми отримаємо виведення тексту з усіх посилань, що містяться на сторінці https://www.example.com.

## Глибші відомості

Використання бібліотеки Jsoup дозволяє працювати з HTML не тільки для отримання текстової інформації, але і для витягування URL адрес, мета тегів, зображень та багато іншого. Детальну інформацію про можливості бібліотеки Jsoup можна знайти на їхньому офіційному сайті: https://jsoup.org/.

## Дивіться також

- [Інструкція по використанню бібліотеки Jsoup](https://jsoup.org/cookbook/introduction/parsing-a-document)
- [Приклади використання бібліотеки для парсингу даних](https://code.tutsplus.com/tutorials/html-parsing-and-screen-scraping-with-the-java-se-api--cms-24169)
- [Порівняння різних бібліотек для парсингу HTML в Java](https://dzone.com/articles/java-html-parsers-performance-comparison)