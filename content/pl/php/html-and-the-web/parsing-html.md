---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:44.264362-07:00
description: "Parsowanie HTML w PHP polega na wydobywaniu okre\u015Blonych informacji\
  \ z dokument\xF3w HTML. Programi\u015Bci wykonuj\u0105 to zadanie, aby automatyzowa\u0107\
  \ ekstrakcj\u0119\u2026"
lastmod: '2024-03-13T22:44:35.495000-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML w PHP polega na wydobywaniu okre\u015Blonych informacji\
  \ z dokument\xF3w HTML. Programi\u015Bci wykonuj\u0105 to zadanie, aby automatyzowa\u0107\
  \ ekstrakcj\u0119\u2026"
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?
Parsowanie HTML w PHP polega na wydobywaniu określonych informacji z dokumentów HTML. Programiści wykonują to zadanie, aby automatyzować ekstrakcję danych, web scraping, lub integrować zawartość z różnych stron internetowych w swoich aplikacjach, zwiększając funkcjonalność bez ręcznej interwencji.

## Jak to zrobić:
Do parsowania HTML, programiści PHP mogą wykorzystać wbudowane funkcje lub opierać się na solidnych bibliotekach takich jak Simple HTML DOM Parser. Tutaj zbadamy przykłady używając zarówno klasy `DOMDocument` PHP, jak i Simple HTML DOM Parser.

### Używanie `DOMDocument`:
Klasa `DOMDocument` w PHP jest częścią rozszerzenia DOM, umożliwiając parsowanie i manipulowanie dokumentami HTML i XML. Oto szybki przykład, jak użyć `DOMDocument` do znalezienia wszystkich obrazów w dokumencie HTML:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Przykładowa strona</title>
</head>
<body>
    <img src="image1.jpg" alt="Obraz 1">
    <img src="image2.jpg" alt="Obraz 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

Przykładowy wynik:
```
image1.jpg
image2.jpg
```

### Używanie Simple HTML DOM Parser:
Do bardziej złożonych zadań lub łatwiejszej składni, można preferować użycie biblioteki zewnętrznej. Simple HTML DOM Parser jest popularnym wyborem, oferując interfejs podobny do jQuery do nawigowania i manipulowania strukturami HTML. Oto jak go użyć:

Najpierw zainstaluj bibliotekę za pomocą Composera:
```
composer require simple-html-dom/simple-html-dom
```

Następnie manipuluj HTML, aby na przykład odnaleźć wszystkie linki:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Ten fragment kodu pobierze zawartość HTML z 'http://www.example.com', przeanalizuje ją i wydrukuje wszystkie hiperłącza. Pamiętaj, aby zamienić `'http://www.example.com'` na faktyczny URL, który chcesz przeanalizować.

Wykorzystując te metody, programiści PHP mogą skutecznie parsować zawartość HTML, dostosowywać ekstrakcję danych do swoich potrzeb lub bezproblemowo integrować zewnętrzną zawartość internetową w swoich projektach.
