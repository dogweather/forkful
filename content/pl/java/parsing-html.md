---
title:                "Wyszukiwanie html"
html_title:           "Java: Wyszukiwanie html"
simple_title:         "Wyszukiwanie html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie HTML to proces analizowania kodu źródłowego strony internetowej w celu wydobycia danych z określonych tagów HTML. Programiści używają tej techniki do automatycznego przetwarzania i zbierania danych z wielu stron internetowych, co może znacznie ułatwić pracę z dużymi zbiorami informacji.

## Jak to zrobić:
```Java
// Importuj potrzebne pakiety
import org.jsoup.Jsoup; 
import org.jsoup.nodes.Document; 
import org.jsoup.nodes.Element; 
import org.jsoup.select.Elements; 

// Pobierz kod źródłowy strony internetowej
Document doc = Jsoup.connect("https://www.example.com").get();

// Wybierz elementy z określonym tagiem HTML i wyświetl ich zawartość
Elements links = doc.select("a");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

Wyjście:
```
Link: https://www.example.com/about
Link: https://www.example.com/products
Link: https://www.example.com/contact
```

## Wnikliwe spojrzenie:
Parsowanie HTML jest często wykorzystywane w aplikacjach do sczytywania informacji z internetu, takich jak agregatory wiadomości czy narzędzia do automatycznego pobierania danych. Alternatywnym sposobem na przetwarzanie stron internetowych może być użycie narzędzi takich jak Selenium lub regularne wyrażenia, jednak parsowanie HTML jest najbardziej skutecznym sposobem na analizowanie treści stron w większej skali.

## Zobacz także:
- [Dokumentacja Jsoup](https://jsoup.org/)
- [Porównanie Selenium i Jsoup](https://www.blazemeter.com/blog/selenium-vs-jsoup-which-one-should-you-choose/) 
- [Przykłady działania na stronie Codecademy](https://www.codecademy.com/articles/how-to-parse-html)