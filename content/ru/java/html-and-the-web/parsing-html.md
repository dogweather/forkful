---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:02.624849-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u043E\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F Jsoup, \u0443\u0434\u043E\
  \u0431\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u043E\
  \u0439 \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0440\u0435\
  \u0430\u043B\u044C\u043D\u044B\u043C HTML. \u0421\u043D\u0430\u0447\u0430\u043B\u0430\
  \ \u0434\u043E\u0431\u0430\u0432\u0438\u043C \u0437\u0430\u0432\u0438\u0441\u0438\
  \u043C\u043E\u0441\u0442\u044C."
lastmod: '2024-03-13T22:44:44.816561-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u043E\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F Jsoup, \u0443\u0434\u043E\u0431\
  \u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u043E\u0439\
  \ \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0440\u0435\u0430\
  \u043B\u044C\u043D\u044B\u043C HTML."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Давайте воспользуемся Jsoup, удобной библиотекой для работы с реальным HTML. Сначала добавим зависимость:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Теперь перейдем к интересной части. Вот как получить заголовок веб-страницы и вывести его:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Заголовок: " + title);
    }
}
```

Вывод:

```
Заголовок: Пример Домена
```

Как насчет извлечения всех ссылок?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... внутри main или другого метода
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Ссылка: " + link.attr("href"));
}
```

## Глубокое погружение
Когда-то HTML был укрощен с помощью регулярных выражений, метода, полного ошибок и кошмарного для сложных документов. На смену пришел Jsoup в конце нулевых, предоставив интерфейс похожий на jQuery для Java, чтобы парсить, переходить и манипулировать HTML.

Jsoup не единственный выбор. Есть HtmlUnit для полноценного тестирования веб-приложений с поддержкой JavaScript, но он более тяжеловесный и сложный. Для легковесных задач отлично подходит Apache Commons Validator, прекрасный инструмент исключительно для извлечения URL.

Под капотом Jsoup использует DOM-парсер, который моделирует весь документ в памяти как дерево. Этот подход облегчает выбор и навигацию по структуре HTML. К тому же он снисходителен к небрежному HTML, исправляя проблемы на лету, чтобы обеспечить надежный парсинг.

Помните, при скрапинге всегда проверяйте файл `robots.txt` и условия предоставления услуг сайта, чтобы избежать юридических проблем или блокировки по IP.

## Смотрите также
- Официальная документация Jsoup: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
