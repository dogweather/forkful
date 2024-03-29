---
date: 2024-01-26 01:56:59.216534-07:00
description: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 \u2014\
  \ \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043F\u0435\u0440\u0435\u0431\
  \u0443\u0434\u043E\u0432\u0438 \u0456\u0441\u043D\u0443\u044E\u0447\u043E\u0433\u043E\
  \ \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\u0440\u043D\u043E\u0433\u043E \u043A\
  \u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\u043C\u0456\u043D\u0438 \u0439\u043E\
  \u0433\u043E \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u044C\u043E\u0457 \u043F\
  \u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0434\u0430\u044E\u0442\u044C\u0441\u044F\
  \ \u0434\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433\
  \u0443, \u0449\u043E\u0431 \u043F\u043E\u043B\u0456\u043F\u0448\u0438\u0442\u0438\
  \u2026"
lastmod: '2024-03-13T22:44:49.444695-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 \u2014\
  \ \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043F\u0435\u0440\u0435\u0431\
  \u0443\u0434\u043E\u0432\u0438 \u0456\u0441\u043D\u0443\u044E\u0447\u043E\u0433\u043E\
  \ \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\u0440\u043D\u043E\u0433\u043E \u043A\
  \u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\u043C\u0456\u043D\u0438 \u0439\u043E\
  \u0433\u043E \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u044C\u043E\u0457 \u043F\
  \u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0434\u0430\u044E\u0442\u044C\u0441\u044F\
  \ \u0434\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433\
  \u0443, \u0449\u043E\u0431 \u043F\u043E\u043B\u0456\u043F\u0448\u0438\u0442\u0438\
  \u2026"
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
---

{{< edit_this_page >}}

## Що та Чому?
Рефакторинг — це процес перебудови існуючого комп'ютерного коду без зміни його зовнішньої поведінки. Програмісти вдаються до рефакторингу, щоб поліпшити неналежні атрибути програмного забезпечення, роблячи код чистішим, ефективнішим і легшим для підтримки.

## Як це робити:
Давайте візьмемо класичний уривок PHP-коду та застосуємо до нього деякі прийоми рефакторингу.

До рефакторингу наш код може виглядати так:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

Але ми можемо рефакторити цей код, щоб покращити його чіткість та модульність:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
Розділяючи функцію `printOrderDetails` на декілька менших функцій, наш код стає зрозумілішим і легшим для налагодження.

## Поглиблено
Рефакторинг має свої корені в спільноті програмування Smalltalk на початку 1990-х років та був додатково популяризований завдяки фундаментальній книзі Мартіна Фаулера "Refactoring: Improving the Design of Existing Code" (1999). Хоча рефакторинг може бути застосований до будь-якої мови програмування, динамічна природа PHP дозволяє стикнутися з деякими унікальними викликами та можливостями.

Альтернативи рефакторингу можуть включати повне переписування коду з нуля, що часто є більш ризикованим і займає більше часу. У екосистемі PHP інструменти на кшталт PHPStan і Rector можуть автоматично виявляти та виконувати деякі операції рефакторингу відповідно. З точки зору реалізації, ключовими практиками є здійснення невеликих рефакторингів і ретельне тестування за допомогою модульних тестів, щоб забезпечити успішний рефакторинг без введення помилок.

## Дивіться також
- Книга Мартіна Фаулера про рефакторинг: https://martinfowler.com/books/refactoring.html
- PHPStan, інструмент статичного аналізу PHP: https://phpstan.org/
- Rector, інструмент для автоматичного рефакторингу коду PHP: https://getrector.org/
- Модульне тестування PHP за допомогою PHPUnit: https://phpunit.de/
