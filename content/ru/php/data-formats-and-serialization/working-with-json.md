---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:06.030438-07:00
description: "JSON (JavaScript Object Notation - \u041D\u043E\u0442\u0430\u0446\u0438\
  \u044F \u041E\u0431\u044A\u0435\u043A\u0442\u0430 JavaScript) - \u044D\u0442\u043E\
  \ \u043B\u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\
  \u043D\u044B\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u0435\u0433\u043E, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\
  \u043D \u043B\u0435\u0433\u043A\u043E\u2026"
lastmod: 2024-02-19 22:05:04.212448
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation - \u041D\u043E\u0442\u0430\u0446\u0438\u044F\
  \ \u041E\u0431\u044A\u0435\u043A\u0442\u0430 JavaScript) - \u044D\u0442\u043E \u043B\
  \u0435\u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\
  \u0430\u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\
  \u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0435\u0433\
  \u043E, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D \u043B\
  \u0435\u0433\u043A\u043E\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
---

{{< edit_this_page >}}

## Что и Почему?
JSON (JavaScript Object Notation - Нотация Объекта JavaScript) - это легковесный формат обмена данными. Программисты используют его, потому что он легко читается и записывается, а также независим от языка, что делает его идеальным для API и веб-сервисов.

## Как это сделать:

### Кодирование массива в JSON
```php
$array = ['foo' => 'bar', 'baz' => 'qux'];
$json = json_encode($array);
echo $json; // {"foo":"bar","baz":"qux"}
```

### Декодирование JSON в объект
```php
$json = '{"foo":"bar","baz":"qux"}';
$object = json_decode($json);
echo $object->foo; // bar
```

### Декодирование JSON в ассоциативный массив
```php
$json = '{"foo":"bar","baz":"qux"}';
$array = json_decode($json, true);
echo $array['foo']; // bar
```

### Обработка ошибок JSON
```php
$json = '{"foo":"bar,"baz":"qux"}'; // Обратите внимание на отсутствующую кавычку
$array = json_decode($json, true);

if(json_last_error() != JSON_ERROR_NONE) {
   echo json_last_error_msg(); // Синтаксическая ошибка, неправильно сформированный JSON
}
```

## Подробнее

JSON является фактическим стандартом для обмена данными в Интернете с начала 2000-х годов, заменяя XML благодаря своей простоте. Существуют альтернативы, такие как XML и YAML, но компактность и скорость JSON сделали его первым выбором. Функции PHP `json_encode()` и `json_decode()` сериализуют и десериализуют данные соответственно. Начиная с PHP 5.4.0, опция `JSON_PRETTY_PRINT` делает вывод более читаемым, а начиная с PHP 7.3.0, разработчики могут выбрасывать `JsonException` для обработки ошибок, делая разбор JSON более надежным.

## Смотрите также

- Руководство по PHP о JSON: https://www.php.net/manual/ru/book.json.php
- Домашняя страница JSON: http://json.org/
- PHP Правильный Путь (раздел обработки JSON): https://phptherightway.com/#json
- Composer, менеджер зависимостей для PHP (использует JSON для информации о пакете): https://getcomposer.org/
