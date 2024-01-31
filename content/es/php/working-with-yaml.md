---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Trabajar con YAML en PHP implica manipular datos en un formato legible y sencillo de entender para humanos. Los programadores lo usan para configuraciones, intercambio de datos y porque es fácil de parsear y generar.

## Cómo se hace:

Para trabajar con YAML en PHP, necesitas la extensión `yaml`. Si no la tienes, instálala usando PECL: `pecl install yaml`. Aquí tienes ejemplos de cómo leer y escribir YAML:

```PHP
<?php
// Para analizar YAML en PHP
$yaml_string = "
nombre: Juan
edad: 30
hobbies:
    - programar
    - leer
";
$array = yaml_parse($yaml_string);
print_r($array);

// Resulta en:
Array
(
    [nombre] => Juan
    [edad] => 30
    [hobbies] => Array
        (
            [0] => programar
            [1] => leer
        )

)

// Para generar YAML en PHP
$array = [
    'nombre' => 'Juan',
    'edad' => 30,
    'hobbies' => ['programar', 'leer']
];
$yaml = yaml_emit($array);
echo $yaml;

// Resulta en:
nombre: Juan
edad: 30
hobbies:
    - programar
    - leer
```

## Profundización

YAML, que significa "YAML Ain't Markup Language", se diseñó en 2001 como alternativa a XML y otros formatos de serialización. Se destaca por su facilidad de lectura y se utiliza comúnmente en configuración de aplicaciones.
Existen alternativas como JSON o XML; sin embargo, YAML es preferido cuando se desea una mayor legibilidad. En PHP, se maneja YAML principalmente a través de la extensión `yaml`, que utiliza `libyaml`, una librería escrita en C para el procesamiento de YAML, para un análisis y emisión rápidos.

## Ver también

- Documentación oficial de PHP para YAML: https://www.php.net/manual/es/book.yaml.php
- Página de YAML: https://yaml.org
- Libreria `libyaml`: https://github.com/yaml/libyaml
