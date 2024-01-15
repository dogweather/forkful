---
title:                "Analiza składni html"
html_title:           "PHP: Analiza składni html"
simple_title:         "Analiza składni html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto zajmuje się tworzeniem stron internetowych lub pracuje w dziedzinie przetwarzania danych w internecie, z pewnością będzie musiał zmierzyć się z analizowaniem kodu HTML. W takich sytuacjach niezbędne jest posiadanie umiejętności parsowania HTML, aby wydobyć potrzebne informacje i przetworzyć je w czytelny dla maszyny format.

## Jak to zrobić

Do parsowania HTML w języku PHP wykorzystywana jest funkcja "file_get_contents()", która pobiera zawartość strony internetowej w postaci kodu HTML. Następnie, możemy wykorzystać funkcję "preg_match_all()", aby przefiltrować kod i odnaleźć żądane elementy.

```PHP
<?php
$html = file_get_contents('https://www.example.com');
preg_match_all('/<p>(.*?)<\/p>/', $html, $paragraphs);
print_r($paragraphs[1]);
```
Przykładowy kod powyżej wyświetli na stronie internetowej wszystkie odnalezione paragrafy i ich zawartość. Możemy także wykorzystać inne wyrażenia regularne, aby dopasować do konkretnych tagów lub atrybutów.

## Pełne zanurzenie

Podczas parsowania HTML ważne jest, aby pamiętać o rzeczach takich jak kontrola błędów, obsługa wyjątków i unikanie przetwarzania zbyt dużej ilości danych na raz. Możemy także wykorzystać gotowe biblioteki, takie jak "PHP Simple HTML DOM Parser", aby ułatwić i usprawnić naszą pracę z HTML.

See Also
- [Funkcja file_get_contents() w PHP](https://www.php.net/manual/en/function.file-get-contents.php)
- [Podstawy wyrażeń regularnych w PHP](https://www.php.net/manual/en/book.pcre.php)
- [PHP Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io/)