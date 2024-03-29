---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:31.902641-07:00
description: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\
  \u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PHP\
  \ \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u0431\u0430\u0432\
  \u043B\u0435\u043D\u0438\u0435 \u043E\u0442 \u044D\u0442\u0438\u0445 \u043D\u0430\
  \u0434\u043E\u0435\u0434\u043B\u0438\u0432\u044B\u0445 \u0434\u0432\u043E\u0439\u043D\
  \u044B\u0445 (`\"`) \u0438\u043B\u0438 \u043E\u0434\u0438\u043D\u0430\u0440\u043D\
  \u044B\u0445 (`'`) \u043A\u0430\u0432\u044B\u0447\u0435\u043A, \u043A\u043E\u0442\
  \u043E\u0440\u044B\u0435 \u043C\u043E\u0433\u0443\u0442 \u0432\u043B\u0438\u044F\
  \u0442\u044C \u043D\u0430 \u043B\u043E\u0433\u0438\u043A\u0443 \u0432\u0430\u0448\
  \u0435\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:45.187777-06:00'
model: gpt-4-0125-preview
summary: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\
  \u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PHP\
  \ \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u0431\u0430\u0432\
  \u043B\u0435\u043D\u0438\u0435 \u043E\u0442 \u044D\u0442\u0438\u0445 \u043D\u0430\
  \u0434\u043E\u0435\u0434\u043B\u0438\u0432\u044B\u0445 \u0434\u0432\u043E\u0439\u043D\
  \u044B\u0445 (`\"`) \u0438\u043B\u0438 \u043E\u0434\u0438\u043D\u0430\u0440\u043D\
  \u044B\u0445 (`'`) \u043A\u0430\u0432\u044B\u0447\u0435\u043A, \u043A\u043E\u0442\
  \u043E\u0440\u044B\u0435 \u043C\u043E\u0433\u0443\u0442 \u0432\u043B\u0438\u044F\
  \u0442\u044C \u043D\u0430 \u043B\u043E\u0433\u0438\u043A\u0443 \u0432\u0430\u0448\
  \u0435\u0433\u043E\u2026"
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и зачем?
Удаление кавычек из строки в PHP означает избавление от этих надоедливых двойных (`"`) или одинарных (`'`) кавычек, которые могут влиять на логику вашего кода или запросы к базе данных. Программисты делают это, чтобы очистить или санитизировать входные данные, гарантируя, что строки безопасно используются или хранятся.

## Как:
Вот простой пример с использованием встроенных функций PHP:

```php
$quotedString = "'Привет,' она сказала, \"Это прекрасный день!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Выведет: Привет, она сказала, Это прекрасный день!
```

Просто, правда? Функция `str_replace()` принимает массив символов, которые нужно удалить из строки, включая как одинарные, так и двойные кавычки.

## Глубже
В ранние времена PHP разработчикам приходилось быть особенно осторожными с кавычками в строках, особенно при вставке данных в базу данных. Неправильное обращение с кавычками могло привести к атакам через SQL-инъекции. Введение магических кавычек, функции, которая автоматически экранировала входные данные, стало решением. Но она была признана устаревшей и в конечном итоге удалена, так как способствовала плохим практикам программирования и проблемам с безопасностью.

Теперь мы используем функции, такие как `str_replace()`, или регулярные выражения с `preg_replace()` для обработки более сложных шаблонов. Вот пример с регулярным выражением:

```php
$quotedString = "'Привет,' она сказала, \"Это прекрасный день!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Для данных в формате JSON вы можете использовать `json_encode()` с опциями вроде `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`, чтобы избежать лишних обратных слешей в кавычках.

При реализации рассмотрите крайние случаи. Что, если в вашей строке должны быть определенные кавычки, как в диалогах в истории или в измерениях в дюймах? Контекст важен, поэтому адаптируйте удаление кавычек к предполагаемому использованию данных.

## Смотрите также
- [PHP: str_replace](https://www.php.net/manual/ru/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/ru/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/ru/function.json-encode.php)
- [OWASP: Предотвращение SQL-инъекций](https://owasp.org/www-community/attacks/SQL_Injection)
