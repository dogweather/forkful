---
title:                "PHP: Parsowanie html"
simple_title:         "Parsowanie html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsowanie HTML to ważna umiejętność, która jest niezbędna dla każdego programisty PHP. Jest to proces konwertowania niezorganizowanych danych HTML na czytelną i przetworzoną formę, co jest niezbędne do tworzenia dynamicznych i interaktywnych stron internetowych. W dzisiejszych czasach, gdzie większość informacji jest dostępna w formie HTML, posiadanie umiejętności parsowania tego języka jest niezwykle ważne.

## Jak to zrobić

Parsowanie HTML w PHP jest stosunkowo proste dzięki różnym bibliotekom i narzędziom dostępnym dla programistów. W poniższym przykładzie wykorzystamy bibliotekę Simple HTML DOM, która pozwala na łatwy dostęp do struktury HTML i jej elementów.

```PHP
<?php // parsowanie HTML za pomocą biblioteki Simple HTML DOM
include_once "simple_html_dom.php";
$html = file_get_html('http://www.example.com/');

// znajdujemy i wyświetlamy tytuł strony
$title = $html->find('title', 0);
echo $title->plaintext;

// znajdujemy i wyświetlamy wszystkie linki na stronie
foreach($html->find('a') as $link){
    echo $link->href . '<br>';
}
```

Przykładowy output:

`<p>Example Domain</p>
https://www.iana.org/domains/example`

W powyższym kodzie najpierw importujemy bibliotekę Simple HTML DOM, a następnie pobieramy zawartość strony internetowej za pomocą funkcji `file_get_html()`. Następnie możemy użyć metody `find()` do znajdowania konkretnych elementów HTML za pomocą selektorów CSS. Dokumentacja biblioteki zawiera pełną listę dostępnych metod i przykładów użycia.

## Deep Dive

Parsowanie HTML może być trochę bardziej skomplikowane, gdy struktura dokumentu jest bardziej złożona. W takich przypadkach przydatne może być wykorzystanie parserów, które pozwalają na dostęp do specjalnych atrybutów HTML, takich jak `data-attributes` czy `class`.

Ponadto, warto zauważyć, że istnieje wiele innych bibliotek do parsowania HTML, takich jak PHP Simple HTML DOM Parser, HTML Purifier czy PHP DOM. Warto zaznaczyć, że każda z nich ma swoje zalety i możliwości, więc warto wybrać tę, która najlepiej odpowiada potrzebom projektu.

## Zobacz także

Jeśli jesteś zainteresowany dalszym zgłębianiem tematu parsowania HTML w PHP, poniżej znajdują się przydatne linki:

- [Dokumentacja biblioteki Simple HTML DOM](https://simplehtmldom.sourceforge.io/)
- [Poradnik do parsowania HTML w PHP](https://www.pluralsight.com/guides/parsing-html-with-php)
- [Przykładowe projekty wykorzystujące parser HTML w PHP](https://github.com/serbanghita/Mobile-Detect)