---
title:                "Trabajando con json"
html_title:           "PHP: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con JSON en PHP significa manejar datos en un formato que es fácil de leer y escribir para los humanos, y también fácil de procesar para las computadoras. Los programadores utilizan JSON para intercambiar datos entre diferentes sistemas o para almacenar datos en una base de datos de una manera organizada y eficiente.

## ¿Cómo hacerlo?
En PHP, podemos utilizar las funciones `json_encode()` y `json_decode()` para convertir datos de y hacia el formato JSON. Por ejemplo, si tenemos un array en PHP, podemos convertirlo a JSON utilizando `json_encode()`, y si tenemos una cadena JSON, podemos convertirla a un array utilizando `json_decode()`.

```
$arr = ['nombre' => 'Juan', 'edad' => 25];
$json = json_encode($arr);
echo $json; // Salida: {"nombre":"Juan","edad":25}

$newArr = json_decode($json);
echo $newArr['nombre']; // Salida: Juan
echo $newArr['edad']; // Salida: 25 
```

## Profundizando
JSON, que significa JavaScript Object Notation, es un formato de texto sencillo que fue desarrollado originalmente por Douglas Crockford en la década de 1990. Se basa en la sintaxis de los objetos de JavaScript y se ha vuelto muy popular en la transferencia de datos en la web debido a su simplicidad y facilidad de uso. Alternativas a JSON incluyen formatos como XML y CSV, pero JSON es más ligero y más fácil de leer y escribir para los humanos.

Además de las funciones `json_encode()` y `json_decode()`, PHP también proporciona la función `json_last_error()` para manejar errores al convertir datos a o desde JSON. También podemos utilizar la opción `JSON_PRETTY_PRINT` en `json_encode()` para obtener una cadena JSON con formato y más fácil de leer para los humanos.

## Ver también
- Documentación oficial de PHP - [Funciones JSON](https://www.php.net/manual/es/ref.json.php)
- Introducción a JSON en W3Schools - [Introducción a JSON](https://www.w3schools.com/js/js_json_intro.asp)
- Tutorial de JSON en Codecademy - [Manipulación de objetos JSON en PHP](https://www.codecademy.com/learn/learn-php/modules/learn-php-arrays/cheatsheet)