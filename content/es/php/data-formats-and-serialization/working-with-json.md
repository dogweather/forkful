---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:02.551400-07:00
description: "C\xF3mo hacerlo: Trabajar con JSON en PHP es sencillo gracias a las\
  \ funciones incorporadas `json_encode()` y `json_decode()`. A continuaci\xF3n, se\
  \ muestran\u2026"
lastmod: '2024-03-13T22:44:59.181812-06:00'
model: gpt-4-0125-preview
summary: Trabajar con JSON en PHP es sencillo gracias a las funciones incorporadas
  `json_encode()` y `json_decode()`.
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
Trabajar con JSON en PHP es sencillo gracias a las funciones incorporadas `json_encode()` y `json_decode()`. A continuación, se muestran ejemplos que destacan cómo convertir un array de PHP en una cadena JSON, y viceversa:

### Codificar un Array de PHP en una Cadena JSON
```php
// Definir un array asociativo
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Convertir el array de PHP a una cadena JSON
$jsonString = json_encode($data);

// Mostrar la cadena JSON
echo $jsonString;
```
**Salida de Muestra:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### Decodificar una Cadena JSON a un Array de PHP
```php
// Cadena JSON
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// Convertir la cadena JSON a un array de PHP
$data = json_decode($jsonString, true);

// Mostrar el array de PHP
print_r($data);
```
**Salida de Muestra:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### Trabajar con una Biblioteca de Terceros: GuzzleHttp
Para el manejo complejo de JSON y solicitudes web, una biblioteca PHP popular es GuzzleHttp. Simplifica las solicitudes HTTP y trabaja fácilmente con datos JSON.

**Instalación vía Composer:**
```
composer require guzzlehttp/guzzle
```

**Ejemplo de Solicitud:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Enviar una solicitud a una API que devuelve JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Decodificar la respuesta JSON a un array de PHP
$data = json_decode($response->getBody(), true);

// Mostrar los datos
print_r($data);
```

**Suponiendo que la API devuelve datos JSON similares:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
Esto muestra la facilidad de usar PHP para manipulación de JSON, tanto con funciones nativas como con bibliotecas robustas como GuzzleHttp para tareas más complejas.
