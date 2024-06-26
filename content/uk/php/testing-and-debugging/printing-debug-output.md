---
date: 2024-01-20 17:53:00.596011-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\u0431\
  \ - \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\
  \u0442\u0438 `echo` \u0430\u0431\u043E `print` \u0434\u043B\u044F \u0432\u0438\u0432\
  \u0435\u0434\u0435\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445."
lastmod: '2024-03-13T22:44:49.434472-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\u0441\u0456\
  \u0431 - \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\
  \u0430\u0442\u0438 `echo` \u0430\u0431\u043E `print` \u0434\u043B\u044F \u0432\u0438\
  \u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445."
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

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
