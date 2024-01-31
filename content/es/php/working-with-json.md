---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Trabajar con JSON (JavaScript Object Notation) significa manejar datos estructurados en un formato ligero y fácil de entender. Los programadores lo usan porque es ampliamente aceptado en web APIs y servicios, facilitando el intercambio de datos entre distintos lenguajes de programación.

## Cómo hacerlo:

### Codificar un array a JSON:
```PHP
$array = ['nombre' => 'Juan', 'edad' => 30, 'ciudad' => 'Madrid'];
$json = json_encode($array);
echo $json;
```
Salida:
```
{"nombre":"Juan","edad":30,"ciudad":"Madrid"}
```

### Decodificar JSON a PHP:
```PHP
$json = '{"nombre":"Juan","edad":30,"ciudad":"Madrid"}';
$array = json_decode($json, true);
print_r($array);
```
Salida:
```
Array ( [nombre] => Juan [edad] => 30 [ciudad] => Madrid )
```

## Profundizar:

JSON, que significa Notación de Objeto de JavaScript, ha existido desde principios de los años 2000 y se ha vuelto casi indispensable en el desarrollo moderno de aplicaciones web, desplazando formatos como XML por su simplicidad. Alternativas como YAML o BSON existen, pero JSON prevalece por su capacidad de ser interpretado fácilmente en muchos lenguajes y su soporte nativo en JavaScript. En PHP, `json_encode()` y `json_decode()` son las funciones principales para trabajar con JSON, manejando conversiones entre arrays/objetos PHP y cadenas JSON.

## Ver También:

- Documentación oficial de PHP para JSON: [php.net/manual/es/book.json.php](https://www.php.net/manual/es/book.json.php)
- JSON.org, con recursos y especificaciones: [json.org](https://json.org/)
- Consejos sobre buenas prácticas con JSON: [jsonapi.org](https://jsonapi.org/)
