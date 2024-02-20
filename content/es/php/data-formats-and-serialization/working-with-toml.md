---
date: 2024-01-26 04:24:32.060428-07:00
description: "TOML, abreviatura de \"Tom's Obvious, Minimal Language\" (Lenguaje M\xED\
  nimo y Obvio de Tom), es un formato de datos similar a JSON o YAML, pero m\xE1s\
  \ f\xE1cil de\u2026"
lastmod: 2024-02-19 22:05:17.691371
model: gpt-4-0125-preview
summary: "TOML, abreviatura de \"Tom's Obvious, Minimal Language\" (Lenguaje M\xED\
  nimo y Obvio de Tom), es un formato de datos similar a JSON o YAML, pero m\xE1s\
  \ f\xE1cil de\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
TOML, abreviatura de "Tom's Obvious, Minimal Language" (Lenguaje Mínimo y Obvio de Tom), es un formato de datos similar a JSON o YAML, pero más fácil de leer para los humanos. Los programadores lo utilizan para archivos de configuración porque es sencillo y se traduce bien a estructuras de datos.

## Cómo hacerlo:
Primero, asegúrate de tener instalada una biblioteca analizadora de TOML, como `yosymfony/toml`. Vamos a analizar un archivo TOML:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[base_de_datos]
servidor = "192.168.1.1"
puertos = [ 8001, 8001, 8002 ]
máximo_de_conexiones = 5000
activado = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

Salida de muestra:

```
Array
(
    [base_de_datos] => Array
        (
            [servidor] => 192.168.1.1
            [puertos] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [máximo_de_conexiones] => 5000
            [activado] => 1
        )

)
```
## Análisis Profundo
TOML surgió en 2013, creado por el cofundador de GitHub, Tom Preston-Werner, como una alternativa más amigable para el usuario a XML y JSON para archivos de configuración. Aunque JSON es sencillo para las máquinas, la estructura de TOML facilita su lectura a los humanos, sin la complejidad de YAML.

Las alternativas a TOML incluyen JSON, YAML y XML. Cada uno tiene sus fortalezas y escenarios de aplicación. JSON es ubicuo e independiente del lenguaje; YAML es más legible y admite comentarios, mientras que XML es extenso y ampliamente soportado.

Al implementar TOML en PHP, estás viendo bibliotecas que analizan su contenido en matrices o objetos de PHP. `yosymfony/toml` es un analizador de PHP que se adhiere a la versión 0.4.0 de la especificación de TOML. Para mantenerse al día, siempre verifica si hay analizadores nuevos o actualizaciones que admitan la versión más actual de TOML (v1.0.0 hasta mi última actualización).

## Ver También
- Especificación de TOML: <https://toml.io/>
- Analizador de TOML para PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Comparación de Formatos de Datos (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Gestor de Paquetes de PHP (Composer): <https://getcomposer.org/>
