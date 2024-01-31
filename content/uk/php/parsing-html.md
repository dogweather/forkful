---
title:                "Парсинг HTML"
date:                  2024-01-20T15:33:03.330136-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Парсинг HTML — це процес витягування даних із структури HTML-документа. Програмісти це роблять для аналізу веб-сторінок, збору інформації та автоматизації веб-інтерактивів.

## How to:

У PHP можна парсити HTML, використовуючи клас `DOMDocument`. Ось як це працює:

```php
<?php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Test Page</title>
</head>
<body>
    <h1>Welcome to my Website!</h1>
    <p>This is a paragraph.</p>
</body>
</html>
HTML;

$dom = new DOMDocument();
@$dom->loadHTML($html);
$titles = $dom->getElementsByTagName('title');

foreach ($titles as $title) {
    echo $title->nodeValue; // Виведе: Test Page
}
?>
```

Ми використовуємо `loadHTML` для завантаження HTML і `getElementsByTagName` для отримання елементів за тегом.

## Deep Dive:

Парсинг HTML у PHP має довгу історію. Раніше це робили з регулярними виразами, але цей метод схильний до помилок. Пізніше почали використовувати `DOMDocument` і бібліотеки типу `SimpleHTMLDom`.

Є альтернативи: `XPath` для складних запитів і `Curl` для завантаження HTML із веб. Один із недоліків `DOMDocument` в PHP - слабка обробка помилок у неправильно сформованому HTML, тому ми використовуємо `@` для придушення помилок.

При роботі з UTF-8 текстом, не забудьте ставити `mb_internal_encoding('UTF-8')` перед парсингом і встановлювати заголовок `Content-type: text/html; charset=utf-8` у HTTP відповіді.

## See Also:

- [PHP DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [XPath Syntax](https://www.w3schools.com/xml/xpath_syntax.asp)
- [SimpleHTMLDom Parser](http://simplehtmldom.sourceforge.net/)
- [Curl with PHP](https://www.php.net/manual/en/book.curl.php)

Всі ці ресурси нададуть вам більше інформації про парсинг HTML та способи роботи з ним у PHP.
