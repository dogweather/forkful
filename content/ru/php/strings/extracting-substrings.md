---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:34.707930-07:00
description: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ \u0432\u044B\u0431\u043E\u0440 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\
  \u043D\u043D\u044B\u0445 \u0447\u0430\u0441\u0442\u0435\u0439 \u0438\u0437 \u0441\
  \u0442\u0440\u043E\u043A\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0438\
  \u043B\u0438 \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438, \u043D\u0430\u043F\u0440\u0438\u043C\
  \u0435\u0440, \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\
  \u044F\u2026"
lastmod: '2024-03-13T22:44:45.189606-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ \u0432\u044B\u0431\u043E\u0440 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\
  \u043D\u043D\u044B\u0445 \u0447\u0430\u0441\u0442\u0435\u0439 \u0438\u0437 \u0441\
  \u0442\u0440\u043E\u043A\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E\
  \ \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0438\
  \u043B\u0438 \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438, \u043D\u0430\u043F\u0440\u0438\u043C\
  \u0435\u0440, \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\
  \u044F\u2026"
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
---

{{< edit_this_page >}}

## Что и Почему?
Извлечение подстрок означает выбор определенных частей из строки. Программисты делают это для обработки или манипуляции данными, например, для получения имени пользователя из адреса электронной почты или расширения файла из названия файла.

## Как это сделать:
PHP предлагает несколько функций для извлечения подстрок. Давайте рассмотрим `substr`, `mb_substr` и `strstr`.

```PHP
$string = "Hello, World! Programming is fun.";

// Извлекаем 'World' используя substr.
echo substr($string, 7, 5); // Вывод: World

// Пример строки UTF-8 с mb_substr для многобайтовых символов.
$utf8String = "こんにちは世界";
echo mb_substr($utf8String, 5, 2); // Вывод: 世

// Получаем все после запятой с помощью strstr.
echo strstr($string, ","); // Вывод: , World! Programming is fun.
```

## Глубже
В начале времен PHP основным способом отделения части строки была функция `substr()`. Однако у `substr()` было (и до сих пор есть) ограничение: она не очень хорошо работает с неанглийскими символами (например, японскими или арабскими).

На сцену выходит `mb_substr()`, безопасный для многобайтных данных аналог, который учитывает символы из различных кодировок. Он гарантирует, что когда вы вытаскиваете подстроку, вы не разрываете символ на середине байта, что критически важно для международных приложений.

`strstr()`, с другой стороны, находит первое вхождение подстроки и дает вам все, что идет после неё. Есть также `strchr()`, который является псевдонимом `strstr()`.

В то время как `substr()` и `mb_substr()` позволяют вам точно указать, с какого места начать и сколько взять, `strstr()` больше похож на инструмент "найди и дай мне остальное".

## Смотрите также
Вот некоторые дополнительные материалы, если вы хотите узнать больше:

- Официальная документация PHP по функциям работы со строками: https://www.php.net/manual/ru/ref.strings.php
- Глубокое погружение в функции работы с многобайтными строками PHP: https://www.php.net/manual/ru/book.mbstring.php
- Больше о кодировании символов и о том, почему это важно: http://kunststube.net/encoding/
