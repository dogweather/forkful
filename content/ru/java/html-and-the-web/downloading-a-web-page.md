---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:31.864447-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041F\u0440\u0438\u043C\u0435\u0440 \u0432\u044B\u0432\u043E\u0434\
  \u0430 \u043C\u043E\u0436\u0435\u0442 \u0432\u044B\u0433\u043B\u044F\u0434\u0435\
  \u0442\u044C \u0442\u0430\u043A."
lastmod: '2024-04-05T21:53:45.363507-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0438\u043C\u0435\u0440 \u0432\u044B\u0432\u043E\u0434\u0430\
  \ \u043C\u043E\u0436\u0435\u0442 \u0432\u044B\u0433\u043B\u044F\u0434\u0435\u0442\
  \u044C \u0442\u0430\u043A."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

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
