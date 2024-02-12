---
title:                "Аналіз HTML"
aliases: - /uk/php/parsing-html.md
date:                  2024-02-03T19:13:11.451836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Парсинг HTML за допомогою PHP полягає у витягуванні певної інформації з HTML-документів. Програмісти виконують це завдання для автоматизації збору даних, веб-скрепінгу або для інтеграції вмісту різних веб-сторінок у свої додатки, покращуючи функціональність без ручного втручання.

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
