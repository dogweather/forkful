---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки - це процес отримання вмісту сторінки з інтернету. Програмісти роблять це, щоб обробляти та аналізувати ці дані у своїх додатках.

## Як це робити:

Використаємо бібліотеку Jsoup для завантаження веб-сторінки. Код зазначено нижче:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class WebDownload {
    public static void main(String[] args) {
        try {
            Document doc = Jsoup.connect("http://example.com").get();
            String title = doc.title();
            System.out.println("Title: " + title);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

При виконанні цього коду, ми отримаємо такий вивід:

```Java
Title: Example Domain
```

## Поглиблено:

Jsoup - це відносно нова бібліотека Java, випущена в 2010 році, яку часто використовують для роботи з HTML в Java. Основні альтернативи - це використання бібліотеки `java.net.URL` для низькорівневого завантаження сторінки або бібліотеки HtmlUnit для більш функціонального, але складнішого варіанту.

Використовуючи Jsoup, ми встановлюємо з'єднання до URL-адреси за допомогою методу `connect()`, потім отримуємо вміст за допомогою методу `get()` і витягуємо заголовок за допомогою методу `title()`. Це всього лише перший крок, адже Jsoup набагато потужніший та надає можливості для парсинга HTML та вибірки елементів за їхніми селекторами CSS.

## Дивіться також:

Для більш детальної інформації про завантаження веб-сторінок та роботу з Jsoup, рекомендуємо ознайомитись із наступними джерелами:

1. [Офіційна документація Jsoup](https://jsoup.org/) 
2. [Проект Jsoup на GitHub](https://github.com/jhy/jsoup)
3. [Як завантажити веб-сторінку з Java - Baeldung](https://www.baeldung.com/java-download-webpage)