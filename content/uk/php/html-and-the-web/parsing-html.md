---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:11.451836-07:00
description: "\u042F\u043A: \u0414\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\
  \u0433\u0443 HTML \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ PHP \u043C\u043E\u0436\u0443\u0442\u044C \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0456 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0430\u0431\u043E\
  \ \u0437\u0432\u0435\u0440\u0442\u0430\u0442\u0438\u0441\u044F \u0434\u043E \u043F\
  \u043E\u0442\u0443\u0436\u043D\u0438\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A, \u044F\u043A-\u043E\u0442 Simple HTML DOM Parser. \u0422\u0443\u0442\
  \ \u043C\u0438\u2026"
lastmod: '2024-03-13T22:44:49.426801-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 HTML\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 PHP \u043C\u043E\
  \u0436\u0443\u0442\u044C \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0456\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0430\u0431\u043E \u0437\u0432\u0435\
  \u0440\u0442\u0430\u0442\u0438\u0441\u044F \u0434\u043E \u043F\u043E\u0442\u0443\
  \u0436\u043D\u0438\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  , \u044F\u043A-\u043E\u0442 Simple HTML DOM Parser."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

## Як:
Для парсингу HTML програмісти PHP можуть використовувати вбудовані функції або звертатися до потужних бібліотек, як-от Simple HTML DOM Parser. Тут ми розглянемо приклади використання як класу `DOMDocument` в PHP, так і Simple HTML DOM Parser.

### Використання `DOMDocument`:
Клас `DOMDocument` в PHP є частиною розширення DOM, що дозволяє парсити та маніпулювати HTML- та XML-документами. Ось швидкий приклад того, як використовувати `DOMDocument` для пошуку всіх зображень у HTML-документі:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Приклад сторінки</title>
</head>
<body>
    <img src="image1.jpg" alt="Зображення 1">
    <img src="image2.jpg" alt="Зображення 2">
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

Приклад виводу:
```
image1.jpg
image2.jpg
```

### Використання Simple HTML DOM Parser:
Для більш складних завдань або для спрощеного синтаксису ви можете віддати перевагу використанню сторонньої бібліотеки. Simple HTML DOM Parser є популярним вибором, надаючи інтерфейс схожий на jQuery для навігації та маніпулювання структурами HTML. Ось як це використовувати:

Спочатку встановіть бібліотеку за допомогою Composer:
```
composer require simple-html-dom/simple-html-dom
```

Потім маніпулюйте HTML, щоб, наприклад, знайти всі посилання:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Цей код завантажить HTML-вміст 'http://www.example.com', проаналізує його та виведе всі гіперпосилання. Не забудьте замінити `'http://www.example.com'` на фактичну URL-адресу, яку ви бажаєте проаналізувати.

Використовуючи ці методи, розробники PHP можуть ефективно парсити HTML-вміст, налаштувати витягування даних відповідно до своїх потреб або безпроблемно інтегрувати зовнішній веб-вміст у свої проєкти.
