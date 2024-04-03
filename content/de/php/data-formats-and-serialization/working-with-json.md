---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:42.859233-07:00
description: "JSON oder JavaScript Object Notation ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das sowohl f\xFCr Menschen einfach zu lesen und zu schreiben als auch\u2026"
lastmod: '2024-03-13T22:44:53.994032-06:00'
model: gpt-4-0125-preview
summary: "JSON oder JavaScript Object Notation ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das sowohl f\xFCr Menschen einfach zu lesen und zu schreiben als auch f\xFCr Maschinen\
  \ einfach zu parsen und zu generieren ist."
title: Arbeiten mit JSON
weight: 38
---

## Wie funktioniert es:
Mit JSON in PHP zu arbeiten, ist dank der integrierten Funktionen `json_encode()` und `json_decode()` unkompliziert. Unten sind Beispiele dargestellt, die zeigen, wie man ein PHP-Array in einen JSON-String umwandelt und umgekehrt:

### Kodierung eines PHP-Arrays in einen JSON-String
```php
// Definiere ein assoziatives Array
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Konvertiere das PHP-Array in einen JSON-String
$jsonString = json_encode($data);

// Ausgabe des JSON-Strings
echo $jsonString;
```
**Beispielausgabe:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### Dekodierung eines JSON-Strings in ein PHP-Array
```php
// JSON-String
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// Konvertiere den JSON-String in ein PHP-Array
$data = json_decode($jsonString, true);

// Ausgabe des PHP-Arrays
print_r($data);
```
**Beispielausgabe:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### Arbeit mit einer Drittanbieter-Bibliothek: GuzzleHttp
Für komplexe JSON- und Webanfragen ist eine beliebte PHP-Bibliothek GuzzleHttp. Sie vereinfacht HTTP-Anfragen und arbeitet problemlos mit JSON-Daten.

**Installation via Composer:**
```
composer require guzzlehttp/guzzle
```

**Beispielanfrage:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Sende eine Anfrage an eine API, die JSON zurückgibt
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Dekodiere die JSON-Antwort in ein PHP-Array
$data = json_decode($response->getBody(), true);

// Ausgabe der Daten
print_r($data);
```

**Angenommen, die API gibt ähnliche JSON-Daten zurück:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
Dies zeigt die Einfachheit der Verwendung von PHP für die JSON-Manipulation, sowohl mit nativen Funktionen als auch mit robusten Bibliotheken wie GuzzleHttp für komplexere Aufgaben.
