---
title:                "Написання тестів"
html_title:           "PHP: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/writing-tests.md"
---

{{< edit_this_page >}}

## Що та чому?
Написання тестів - це процес створення програмного коду, який перевіряє коректність і вірність роботи інших кодів у вашій програмі. Програмісти пишуть тести, щоб переконатися, що їх код працює правильно та надійно.

## Як:
```PHP
<?php
// Приклад тесту з використанням функції assertEquals
class Test extends \PHPUnit\Framework\TestCase {
    public function testAddition() {
        $result = add(2, 3);
        $this->assertEquals(5, $result);
    }
}
// Приклад виклику функції, яка буде перевірятися тестом
function add($a, $b) {
    return $a + $b;
}
?>
```
Ви можете запускати цей тест за допомогою phpunit і переконатися, що він успішний: 
```
> phpunit test.php
OK (1 test, 1 assertion)
```

## Глибоке поглиблення:
Наші тести сьогодні користуються популярністю завдяки парадигмі розробки, яка називається тест-оперованою розробкою (Test-Driven Development). Цей підхід передбачає написання тестів перед написанням справжнього коду і змушує програмістів думати про те, яким повинен бути справжній код, щоб пройти всі тести.

Існують альтернативи підходу Test-Driven Development, наприклад, Behavior-Driven Development (BDD), де тести пишуться у більш розуміному для бізнесу стилі і перевіряють поведінку програми з точки зору користувача.

У реальних проектах важливо писати якомога більшу кількість тестів, щоб забезпечити максимальну вірогідність успішної роботи вашої програми.

## Дивіться також:
[PHPUnit офіційна документація](https://phpunit.de/documentation.html)

[Стаття на тему тестування коду в парадигмі Test-Driven Development](https://www.codeproject.com/articles/6564/test-driven-development-tutorial)