---
title:                "Рефакторинг"
aliases:
- uk/php/refactoring.md
date:                  2024-01-26T01:56:59.216534-07:00
model:                 gpt-4-0125-preview
simple_title:         "Рефакторинг"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/refactoring.md"
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
