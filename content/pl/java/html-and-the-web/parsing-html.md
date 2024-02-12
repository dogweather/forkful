---
title:                "Analiza składniowa HTML"
date:                  2024-02-03T19:12:32.759360-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co & Dlaczego?

Parsowanie HTML oznacza przekopywanie się przez znaczniki w celu wydobycia danych takich jak tekst, linki czy inne elementy. Robimy to, aby interaktywnie obsługiwać lub pobierać zawartość z sieci web, automatyzować zadania przeglądania czy testować aplikacje internetowe.

## Jak to zrobić:

Użyjmy Jsoup, przydatnej biblioteki do pracy z HTML-em z prawdziwego świata. Najpierw dodaj zależność:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Teraz czas na zabawę. Oto jak wydobyć tytuł strony internetowej i go wyświetlić:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Tytuł: " + title);
    }
}
```

Wynik:

```
Tytuł: Przykładowa Domena
```

Co z wydobyciem wszystkich linków?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... wewnątrz metody main lub innej metody
Elementy linków = doc.select("a[href]");
for (Element link : linki) {
    System.out.println("Link: " + link.attr("href"));
}
```

## Dogłębna analiza

Dawniej HTML był okiełznywany przez wzorce regex, metodą zarówno podatną na błędy, jak i koszmarną dla skomplikowanych dokumentów. Wprowadzono Jsoup pod koniec lat 00., oferujący interfejs podobny do jQuery dla Javy do parsowania, przechodzenia i manipulowania HTML-em.

Jsoup to nie jedyny wybór. Istnieje HtmlUnit do kompleksowego testowania aplikacji webowych z obsługą JavaScript, ale jest on cięższy i bardziej skomplikowany. Dla lekkich zadań świetnie nadaje się Apache Commons Validator, doskonały do wydobywania URL-i.

W swoim działaniu Jsoup używa parsera DOM, który modeluje cały dokument w pamięci jako drzewo. To podejście ułatwia wybieranie i nawigowanie po strukturze HTML. Co więcej, jest tolerancyjny wobec niedbałego HTML-a, naprawiając problemy na bieżąco, aby zapewnić solidne parsowanie.

Pamiętaj, podczas skrobania, zawsze sprawdź `robots.txt` strony i warunki świadczenia usług, aby uniknąć problemów prawnych lub zbanowania adresu IP.

## Zobacz również

- Oficjalna dokumentacja Jsoup: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
