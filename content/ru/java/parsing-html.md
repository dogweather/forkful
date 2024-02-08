---
title:                "Разбор HTML"
aliases:
- ru/java/parsing-html.md
date:                  2024-01-29T00:00:02.624849-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Парсинг HTML означает проникновение сквозь разметку для извлечения данных, таких как текст, ссылки или другие элементы. Мы делаем это для взаимодействия с веб-контентом или его скрапинга, автоматизации задач просмотра или тестирования веб-приложений.

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
