---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:08.955919-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PHP \u043F\u043E\u0434\u0440\u0430\
  \u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\
  \u043D\u0438\u0435 \u0432\u0445\u043E\u0434\u043D\u044B\u0445 \u0434\u0430\u043D\
  \u043D\u044B\u0445, \u043F\u0435\u0440\u0435\u0434\u0430\u043D\u043D\u044B\u0445\
  \ \u0432 \u0432\u0430\u0448 \u0441\u043A\u0440\u0438\u043F\u0442 \u043F\u0440\u0438\
  \ \u0435\u0433\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u0438\
  \ \u0432 \u043A\u043E\u043D\u0441\u043E\u043B\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\u2026"
lastmod: '2024-03-13T22:44:45.239291-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PHP \u043F\u043E\u0434\u0440\u0430\
  \u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\
  \u043D\u0438\u0435 \u0432\u0445\u043E\u0434\u043D\u044B\u0445 \u0434\u0430\u043D\
  \u043D\u044B\u0445, \u043F\u0435\u0440\u0435\u0434\u0430\u043D\u043D\u044B\u0445\
  \ \u0432 \u0432\u0430\u0448 \u0441\u043A\u0440\u0438\u043F\u0442 \u043F\u0440\u0438\
  \ \u0435\u0433\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u0438\
  \ \u0432 \u043A\u043E\u043D\u0441\u043E\u043B\u0438. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и Зачем?
Чтение аргументов командной строки в PHP подразумевает получение входных данных, переданных в ваш скрипт при его выполнении в консоли. Программисты делают это, чтобы сделать свои скрипты интерактивными и настраиваемыми без жесткого кодирования значений.

## Как это сделать:
PHP использует глобальный массив `$argv` для хранения аргументов командной строки, причём `$argv[0]` является именем скрипта. Вот как это используется:

```php
<?php
// проверяем, переданы ли какие-либо аргументы
if ($argc > 1) {
    echo "Привет, " . $argv[1] . "!\n";
} else {
    echo "Привет, кто бы ты ни был!\n";
}
?>
```

Если вы назовете этот скрипт `sayhello.php` и запустите `php sayhello.php Мир`, вывод будет:

```
Привет, Мир!
```

Нет аргументов? Получите:

```
Привет, кто бы ты ни был!
```

## Глубокое Погружение
Исторически скрипты командной строки были основой системной автоматизации, задолго до того, как графические интерфейсы взяли верх. PHP, хотя и широко используется для веб-разработки, также предоставляет надежную поддержку CLI. 

Два основных способа чтения аргументов в PHP - это `$argv` и функция `getopt()`. Первый представляет собой простой массив, в то время как `getopt()` предоставляет более сложную функциональность, такую как разбор параметров (со значениями или без).

Что касается реализации, то `$argv` и `$argc` (количество аргументов) автоматически доступны в режиме CLI — не требуется дополнительной настройки. Они не присутствуют при выполнении веб-скриптов PHP, потому что это не их арена.

Но помните, если вы зарегистрируете `argv` и `argc` как глобальные переменные через `php.ini` или конфигурацию сервера, их также можно будет получить в веб-скриптах. Хотя это редко необходимо и может представлять собой риск для безопасности.

## Смотрите также
Для более сложного разбора командной строки:
- [PHP.net getopt](https://www.php.net/manual/ru/function.getopt.php)

Чтобы углубиться в CLI сервер PHP:
- [PHP.net Использование командной строки](https://www.php.net/manual/ru/features.commandline.php)

Взаимодействуйте с сообществом PHP:
- [Обсуждения PHP CLI на Stack Overflow](https://stackoverflow.com/questions/tagged/php+cli)
