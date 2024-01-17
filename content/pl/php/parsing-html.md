---
title:                "Analiza html."
html_title:           "PHP: Analiza html."
simple_title:         "Analiza html."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-html.md"
---

{{< edit_this_page >}}

## Co to jest i po co? 

Parsowanie HTML jest procesem odczytywania i analizowania kodu HTML, który jest używany do tworzenia stron internetowych. Programiści często wykorzystują ten proces do wyciągania konkretnej informacji lub danych z witryny internetowej, co może być przydatne w wielu przypadkach, np. do tworzenia wyszukiwarek lub automatycznych narzędzi do skracania linków.

## Jak to zrobić:

```PHP
// Przykład użycia funkcji do parsowania HTML
$html = file_get_contents("strona.html"); //Odczytanie strony internetowej
$doc = new DOMDocument(); //Utworzenie nowego obiektu DOMDocument
$doc->loadHTML($html); //Załadowanie odczytanego kodu HTML
$xpath = new DOMXPath($doc); //Utworzenie obiektu XPath do przeszukiwania dokumentu
$title = $xpath->query("//title"); //Znalezienie elementu <title> w dokumencie
echo $title[0]->nodeValue; //Wyświetlenie wartości elementu <title> - tytułu strony
```

  *Wyjście:* ```Przykładowa strona internetowa```

## Specyfiki:

Parsowanie HTML jest niezbędnym elementem wielu projektów internetowych. W przeszłości, programiści często korzystali z regexów do analizowania kodu HTML, jednak metoda ta nie jest skuteczna i może prowadzić do błędów. Alternatywą jest wykorzystanie frameworka, takiego jak Simple HTML DOM, który znacznie ułatwia proces parsowania HTML. W implementacji, należy pamiętać o wykorzystaniu metody bezpiecznej wobec ataków XSS, jaką jest funkcja filter_var() w PHP.

## Zobacz też:
- [Simple HTML DOM](https://simplehtmldom.sourceforge.io/)
- [XPath w PHP](https://www.php.net/manual/en/class.domxpath.php)
- [Bezpieczeństwo podczas parsowania HTML](https://www.owasp.org/index.php/Preventing_HTML_Injection_in_Php)