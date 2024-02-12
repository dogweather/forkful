---
title:                "Виведення налагоджувальної інформації"
aliases: - /uk/php/printing-debug-output.md
date:                  2024-01-20T17:53:00.596011-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Програмісти використовують виведення налагоджувального тексту, щоб бачити, що відбувається всередині програми під час її роботи. Це допомагає відстежувати хід виконання програми та знаходити проблеми.

## Як це зробити:
Простий спосіб - використовувати `echo` або `print` для виведення даних.

```PHP
<?php
$variable = "Hello, World!";
echo $variable; // Виведе: Hello, World!

// Виведення змінних з контекстом
$user = 'Oleksiy';
echo "Logged in user: $user"; // Виведе: Logged in user: Oleksiy
?>
```

Для більш складної інформації - `print_r` або `var_dump`:

```PHP
<?php
$array = array('apple', 'banana', 'cherry');
print_r($array);
/*
Виведе:
Array
(
    [0] => apple
    [1] => banana
    [2] => cherry
)
*/

// З більшими деталями
var_dump($array);
/*
Виведе:
array(3) {
  [0]=>
  string(5) "apple"
  [1]=>
  string(6) "banana"
  [2]=>
  string(6) "cherry"
}
*/
?>
```

## Поглиблено:
З часів PHP 4, `print_r` та `var_dump` допомагають програмістам розуміти структуру даних. Хоч `echo` та `print` корисні, вони обмежені. `var_dump` може вивести типи даних та розміри, пропонуючи більше контексту.

Альтернативою є `xdebug` - розширення PHP, яке поліпшує виведення налагоджування, додаючи кольори та стек-трейси.

Також, можна ведення логів в файл за допомогою `error_log()` для збереження налагоджувальних даних без виведення їх на екран.

## Дивись також:
- [PHP Manual - Debugging](https://www.php.net/manual/en/book.errorfunc.php)
- [xdebug - Debugger and Profiler Tool for PHP](https://xdebug.org/)
- [PHP The Right Way - Error Reporting](https://phptherightway.com/#error_reporting)
