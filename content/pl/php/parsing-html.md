---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza składniowa HTML to proces ekstrakcji danych z dokumentów HTML. Programiści wykonują to, aby zdobyć istotne informacje z dokumentu lub manipulować strukturą strony na podstawie konkretnych wymagań.

## Jak to zrobić:

Użyjemy biblioteki `DOMDocument` dostępnej w PHP. Tu jest podstawowy przykład:

```PHP
<?php
$dom = new DOMDocument();
@$dom->loadHTML('<div class="test">Witaj Świecie!</div>');

$divs = $dom->getElementsByTagName('div');
foreach($divs as $div) {
    if($div->getAttribute('class') === 'test') {
        echo $div->nodeValue;
    }
}
?>
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```PHP
Witaj Świecie!
```
## Głębokie zanurzenie:

1) Kontekst historyczny: Przetwarzanie HTML stało się popularne z pojawieniem się dynamicznych stron internetowych, gdzie interakcje użytkowników wymagały manipulacji strukturą HTML.

2) Alternatywy: Inne popularne parsery HTML dla PHP to: `phpQuery`, `Simple HTML DOM Parser`. Każda metoda ma swoje zalety i wady, wybór zależy od specyfikacji projektu.

3) Szczegóły implementacji: `DOMDocument` to biblioteka PHP do parsowania HTML. Co istotne, ignoruje ona błędy podczas wczytywania HTML (dzięki użyciu '@'), co jest bardzo wygodne przy pracy z niedoskonałymi dokumentami HTML.

## Zobacz także:

- Dokumentacja `DOMDocument`: https://www.php.net/manual/en/class.domdocument.php
- Biblioteka `phpQuery`: https://code.google.com/archive/p/phpquery/
- Biblioteka `Simple HTML DOM Parser`: http://simplehtmldom.sourceforge.net/