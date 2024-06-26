---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:17.364203-07:00
description: "Jak to zrobi\u0107: Praca z JSONem w PHP jest prosta dzi\u0119ki wbudowanym\
  \ funkcjom `json_encode()` i `json_decode()`. Poni\u017Cej znajduj\u0105 si\u0119\
  \ przyk\u0142ady pokazuj\u0105ce,\u2026"
lastmod: '2024-03-13T22:44:35.518927-06:00'
model: gpt-4-0125-preview
summary: "Praca z JSONem w PHP jest prosta dzi\u0119ki wbudowanym funkcjom `json_encode()`\
  \ i `json_decode()`."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
Praca z JSONem w PHP jest prosta dzięki wbudowanym funkcjom `json_encode()` i `json_decode()`. Poniżej znajdują się przykłady pokazujące, jak przekształcić tablicę PHP w ciąg JSON i odwrotnie:

### Kodowanie tablicy PHP do ciągu JSON
```php
// Zdefiniuj tablicę asocjacyjną
$data = [
    "name" => "Jan Kowalski",
    "age" => 30,
    "email" => "jan.kowalski@example.com"
];

// Przekształć tablicę PHP na ciąg JSON
$jsonString = json_encode($data);

// Wypisz ciąg JSON
echo $jsonString;
```
**Przykładowe wyjście:**
```json
{"name":"Jan Kowalski","age":30,"email":"jan.kowalski@example.com"}
```

### Dekodowanie ciągu JSON na tablicę PHP
```php
// Ciąg JSON
$jsonString = '{"name":"Jan Kowalski","age":30,"email":"jan.kowalski@example.com"}';

// Przekształć ciąg JSON na tablicę PHP
$data = json_decode($jsonString, true);

// Wypisz tablicę PHP
print_r($data);
```
**Przykładowe wyjście:**
```
Array
(
    [name] => Jan Kowalski
    [age] => 30
    [email] => jan.kowalski@example.com
)
```

### Praca z zewnętrzną biblioteką: GuzzleHttp
Do skomplikowanego obsługiwania JSON i zapytań internetowych popularną biblioteką PHP jest GuzzleHttp. Upraszcza ona zapytania HTTP i łatwo pracuje z danymi JSON.

**Instalacja przez Composera:**
```
composer require guzzlehttp/guzzle
```

**Przykładowe zapytanie:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Wyślij zapytanie do API, które zwraca JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Dekoduj odpowiedź JSON do tablicy PHP
$data = json_decode($response->getBody(), true);

// Wypisz dane
print_r($data);
```

**Zakładając, że API zwraca podobne dane JSON:**
```
Array
(
    [name] => Jan Kowalski
    [age] => 30
    [email] => jan.kowalski@example.com
)
```
To pokazuje łatwość użycia PHP do manipulacji danymi JSON, zarówno za pomocą funkcji natywnych, jak i z rozbudowanymi bibliotekami takimi jak GuzzleHttp do bardziej złożonych zadań.
