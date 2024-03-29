---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:15.810773-07:00
description: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\
  \u043E\u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\
  \u0432\u0443\u044E\u0449\u0438\u0445 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0451\
  \u043D\u043D\u043E\u043C\u0443 \u0448\u0430\u0431\u043B\u043E\u043D\u0443 \u0432\
  \ PHP, \u0437\u0430\u043A\u043B\u044E\u0447\u0430\u0435\u0442\u0441\u044F \u0432\
  \ \u043F\u043E\u0438\u0441\u043A\u0435 \u043A\u043E\u043D\u043A\u0440\u0435\u0442\
  \u043D\u044B\u0445 \u043F\u043E\u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u0435\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0435\u0439 \u0441\u0438\u043C\u0432\u043E\u043B\
  \u043E\u0432 \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445 \u0438 \u0438\u0445\
  \ \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u0438.\u2026"
lastmod: '2024-03-13T22:44:45.180556-06:00'
model: gpt-4-0125-preview
summary: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\
  \u043E\u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\
  \u0432\u0443\u044E\u0449\u0438\u0445 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0451\
  \u043D\u043D\u043E\u043C\u0443 \u0448\u0430\u0431\u043B\u043E\u043D\u0443 \u0432\
  \ PHP, \u0437\u0430\u043A\u043B\u044E\u0447\u0430\u0435\u0442\u0441\u044F \u0432\
  \ \u043F\u043E\u0438\u0441\u043A\u0435 \u043A\u043E\u043D\u043A\u0440\u0435\u0442\
  \u043D\u044B\u0445 \u043F\u043E\u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u0435\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0435\u0439 \u0441\u0438\u043C\u0432\u043E\u043B\
  \u043E\u0432 \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445 \u0438 \u0438\u0445\
  \ \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u0438.\u2026"
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
---

{{< edit_this_page >}}

## Что и Почему?

Удаление символов, соответствующих определённому шаблону в PHP, заключается в поиске конкретных последовательностей символов в строках и их удалении. Программисты делают это для очистки данных, форматирования вывода или манипулирования строками для соответствия определённым критериям, например, удаления не-алфавитно-цифровых символов из пользовательского ввода по соображениям безопасности.

## Как это сделать:

PHP использует функцию `preg_replace` для удаления символов, соответствующих шаблону с использованием регулярных выражений. Вот как удалить цифры из строки:

```PHP
<?php
$text = "Год 2023!";
$pattern = '/\d+/'; // Шаблон для соответствия всем цифрам
$result = preg_replace($pattern, '', $text);
echo $result; // Выводит: Год !
?>
```

А вот как убрать пробелы:

```PHP
<?php
$text = "Слишком   много      пробелов!";
$pattern = '/\s+/'; // Шаблон для соответствия всем пробелам
$result = preg_replace($pattern, ' ', $text);
echo $result; // Выводит: Слишком много пробелов!
?>
```

## Подробнее

Удаление символов, соответствующих шаблонам, не является новинкой. Функция `preg_replace` в PHP, которая обеспечивает эту функциональность, использует регулярные выражения, совместимые с Perl, основной инструмент обработки текста с конца 80-х годов, когда Perl стал популярным. Альтернативы `preg_replace` включают `str_replace` для простых замен и `trim`, `ltrim` и `rtrim` для удаления пробелов из строк. Для более тонких удалений по шаблону можно использовать `preg_replace_callback` для дополнительного контроля в процессе замены.

Полезно знать, что PREG в `preg_replace` означает Perl Regular Expressions, подчёркивающий использование в PHP синтаксиса шаблонов Perl. Вот разбивка:

- `\d` соответствует любой цифре. Добавление `+` означает "один или более" предыдущего элемента (цифры, в данном случае).
- `\s` находит любой пробел. Как и числа, `+` после `\s` нацеливается на длинные участки пространства.

Выбор между `preg_replace` и его альтернативами зависит от того, что вы делаете. Используйте `preg_replace` для сложных шаблонов и `str_replace` при работе с простыми, прямыми заменами.

Помните, неправильное использование регулярных выражений может привести к неэффективному коду. Всегда проводите бенчмарк и используйте регулярные выражения умно.

## Смотрите также

Для получения дополнительной информации о функциях работы со строками и сопоставлении шаблонов в PHP:
- [PHP Manual — preg_replace](https://www.php.net/manual/ru/function.preg-replace.php)
- [PHP Manual — Регулярные выражения (совместимые с Perl)](https://www.php.net/manual/ru/book.pcre.php)
- [PHP Manual — str_replace](https://www.php.net/manual/ru/function.str-replace.php)
- [PHP Manual — Функции работы со строками](https://www.php.net/manual/ru/ref.strings.php)

Эти ссылки ведут к официальной документации PHP, где можно углубиться в детали манипулирования строками и сопоставления шаблонов.
