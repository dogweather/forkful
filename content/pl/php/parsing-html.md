---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:32:59.958768-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsing HTML to proces tłumaczenia kodu HTML na struktury danych, które łatwo przetwarzają programy. Robimy to, aby wydobyć informacje, manipulować zawartością lub integrować rozmaite usługi z istniejącymi stronami internetowymi.

## Jak to zrobić?
PHP ma wbudowany sposób na parsing HTML – rozszerzenie `DOMDocument`. Oto prosty przykład użycia:

```php
<?php
$html = '<!DOCTYPE html><html><body><p>Witaj, Świecie!</p></body></html>';

$dom = new DOMDocument();
@$dom->loadHTML($html);
$paragraphs = $dom->getElementsByTagName('p');

foreach ($paragraphs as $p) {
    echo $p->nodeValue . "\n";
}
?>
```
Wynik:
```
Witaj, Świecie!
```

## A może głębiej?

Historia łamanek HTML sięga lat 90-tych, kiedy to internet zaczął prężnie rosnąć. PHP wspierał różne techniki, ale `DOMDocument` trafiło do mainstreamu.

Inne metody to `SimpleXML` czy używanie wyrażeń regularnych (regex), jednak te ostatnie są niezalecane z powodu skomplikowości HTML.

Implementation parsingu HTML powinna kierować się standardami, tak aby był odporny na zmienny i czasem niepoprawny kod HTML. `libxml` używane przez PHP do obsługi XML i HTML, pomaga sobie z tym znakomicie.

## Zobacz też

- Oficjalna dokumentacja PHP `DOMDocument`: https://www.php.net/manual/en/class.domdocument.php
- Wprowadzenie do `libxml`: https://www.php.net/manual/en/book.libxml.php
- `SimpleXML` — alternatywna, prostsza biblioteka do manipulacji XML: https://www.php.net/manual/en/book.simplexml.php
