---
title:                "Работа с JSON"
aliases:
- ru/php/working-with-json.md
date:                  2024-01-29T00:04:06.030438-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
