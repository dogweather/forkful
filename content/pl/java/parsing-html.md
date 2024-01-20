---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Analiza składniowa HTML polega na interpretacji dokumentów HTML w celu zrozumienia ich struktury i zawartości. Programiści robią to, aby manipulować strukturą strony, ekstrahować dane, testować UX i wiele więcej.

## Jak to zrobić:

Aby zacząć, musimy najpierw dodać odpowiednią bibliotekę do projektu. Będziemy potrzebować biblioteki Jsoup:

```java
// Dodaj to do pliku pom.xml
<dependency>
   <groupId>org.jsoup</groupId>
   <artifactId>jsoup</artifactId>
   <version>1.13.1</version>
</dependency>
```
Teraz możemy zacząć z kodowaniem. Oto przykład jak to zrobić:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class Main {
    public static void main(String[] args) throws Exception {
        Document doc = Jsoup.connect("http://example.com").get();
        Elements newsHeadlines = doc.select("#mp-itn b a");
        for (Element headline : newsHeadlines) {
            System.out.println(headline.attr("title"));
            System.out.println(headline.absUrl("href"));
        }
    }
}
```
Jeśli wszystko zadziała poprawnie, powinniśmy zobaczyć tytuły niektórych artykułów na stronie "example.com"

## Głębsze spojrzenie:

Historia analizy składni HTML jest historycznie związana z rozwojem sieci internetowej i technologii webowych. Przez lata powstało wiele różnych narzędzi do analizy składni HTML, takich jak Beautiful Soup, HtmlUnit, i wiele innych. Jsoup, który omówiliśmy tu, jest znany ze swojej szybkości i wydajności.

Alternatywą dla Jsoup może być HtmlUnit, która jest również mocną biblioteką do analizy składni HTML w Javie. Jeścieś odważny, możesz nawet spróbować napisać własny parser!

Implementacja analizy składni HTML z za pomocą Jsoup polega na zastosowaniu DOM (Document Object Model) do reprezentowania struktury HTML.

## Zobacz także:

- [Dokumentacja Jsoup](https://jsoup.org/cookbook/)
- [Tutorial HtmlUnit](http://htmlunit.sourceforge.net/)