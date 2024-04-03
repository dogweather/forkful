---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:32.122718-07:00
description: "JSON (JavaScript Object Notation) is een lichtgewicht gegevensformaat\
  \ voor gegevensuitwisseling. Programmeurs gebruiken het omdat het makkelijk te\u2026"
lastmod: '2024-03-13T22:44:50.917068-06:00'
model: gpt-4-0125-preview
summary: JSON (JavaScript Object Notation) is een lichtgewicht gegevensformaat voor
  gegevensuitwisseling.
title: Werken met JSON
weight: 38
---

## Wat & Waarom?
JSON (JavaScript Object Notation) is een lichtgewicht gegevensformaat voor gegevensuitwisseling. Programmeurs gebruiken het omdat het makkelijk te lezen/schrijven is en taalonafhankelijk, wat het ideaal maakt voor API's en webdiensten.

## Hoe te:

### Een array naar JSON coderen
```php
$array = ['foo' => 'bar', 'baz' => 'qux'];
$json = json_encode($array);
echo $json; // {"foo":"bar","baz":"qux"}
```

### JSON decoderen naar een object
```php
$json = '{"foo":"bar","baz":"qux"}';
$object = json_decode($json);
echo $object->foo; // bar
```

### JSON decoderen naar een associatieve array
```php
$json = '{"foo":"bar","baz":"qux"}';
$array = json_decode($json, true);
echo $array['foo']; // bar
```

### Omgaan met JSON-fouten
```php
$json = '{"foo":"bar,"baz":"qux"}'; // Let op de ontbrekende aanhalingsteken
$array = json_decode($json, true);

if(json_last_error() != JSON_ERROR_NONE) {
   echo json_last_error_msg(); // Syntaxisfout, slecht gevormde JSON
}
```

## Diepgaand

JSON is sinds de vroege jaren 2000 de de facto standaard voor webgegevensuitwisseling geworden, waarbij XML werd vervangen vanwege de eenvoud. Alternatieven zoals XML en YAML bestaan, maar de compactheid en snelheid van JSON hebben het tot een topkeuze gemaakt. De PHP `json_encode()` en `json_decode()` functies serialiseren en deserializeren gegevens respectievelijk. Sinds PHP 5.4.0 maakt de optie `JSON_PRETTY_PRINT` de uitvoer leesbaarder, en vanaf PHP 7.3.0 kunnen ontwikkelaars `JsonException` gooien voor foutafhandeling, waardoor het parsen van JSON robuuster wordt.

## Zie Ook

- PHP Handleiding over JSON: https://www.php.net/manual/en/book.json.php
- JSON Homepage: http://json.org/
- PHP op de Juiste Manier (JSON handling sectie): https://phptherightway.com/#json
- Composer, een afhankelijkheidsbeheer voor PHP (gebruikt JSON voor pakketinformatie): https://getcomposer.org/
