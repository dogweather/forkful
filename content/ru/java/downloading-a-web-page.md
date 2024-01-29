---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:31.864447-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Загрузка веб-страницы означает программное получение её содержимого, такого как HTML, CSS и JavaScript. Программисты делают это для обработки данных, мониторинга изменений или тестирования своих веб-приложений.

## Как это сделать:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String urlStr = "http://example.com";
        try {
            URL url = new URL(urlStr);
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    System.out.println(line);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Пример вывода может выглядеть так:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## Погружение

В прошлом загрузка веб-страницы была элементарной задачей — HTTP был простым, веб-сайты состояли в основном из статичного HTML. Современный веб сложен — подумайте о HTTPS, содержимом, управляемом JavaScript, и обилии AJAX.

Для статического содержимого `java.net.URL` и `java.net.HttpURLConnection` — это прямые выборы — без излишеств, просто работает. Но если вы нацелены на сайты, полные динамического контента, загружаемого с помощью JavaScript, эти классы в одиночку справиться не смогут, и вы будете смотреть в сторону таких инструментов, как Selenium или HtmlUnit.

Не забывайте, что выбор правильного инструмента также зависит от того, что вам нужно делать со страницей после её загрузки. Разбор HTML? Jsoup ваш выбор. Выполнение JavaScript? Рассмотрите вариант с безголовым браузером. Классы `java.net` — лишь верхушка айсберга, но они подходят для быстрых задач или сбора данных со старомодных веб-страниц.

Помните о политике вежливости: не атакуйте сайт серией быстрых запросов, иначе вы рискуете быть забаненым. И убедитесь, что вы действуете в соответствии с рекомендациями `robots.txt` веб-сайта.

## Смотрите также

- [Библиотека Jsoup](https://jsoup.org/) для разбора и извлечения HTML.
- [Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver/) для более сложных задач, включая выполнение JavaScript.
- Руководство по [HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) для тех, кто хочет узнать тонкости работы с HTTP в Java.
- [HtmlUnit](http://htmlunit.sourceforge.net/), «браузер без графического интерфейса для программ на Java», отлично подходит для страниц с интенсивным использованием JavaScript.
