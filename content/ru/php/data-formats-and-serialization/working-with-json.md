---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:06.030438-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.250045-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
