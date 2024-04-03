---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:02.131422-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos que\
  \ se\u2026"
lastmod: '2024-03-13T22:44:59.180710-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos que\
  \ se utiliza com\xFAnmente para archivos de configuraci\xF3n."
title: Trabajando con YAML
weight: 41
---

## Cómo:
PHP, en sus iteraciones actuales, no admite el análisis de YAML como parte de su biblioteca estándar. La forma más directa de trabajar con YAML en PHP es utilizando el componente YAML de Symfony o la extensión PECL `yaml`.

### Utilizando el Componente YAML de Symfony
Primero, instala el componente YAML de Symfony a través de Composer:

```bash
composer require symfony/yaml
```

Luego, puedes analizar y volcar contenido YAML de la siguiente manera:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Analizando YAML
$yamlString = <<<YAML
greet: Hola, Mundo!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Creando YAML a partir de un array
$array = [
    'greet' => '¡Hola, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Salida de muestra al analizar:

```
Array
(
    [greet] => Hola, Mundo!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Salida de muestra al volcar:

```
greet: ¡Hola, YAML!
framework:
    name: Symfony
    language: PHP
```

### Utilizando la Extensión PECL `yaml`
Si lo prefieres, o si los requisitos de tu proyecto lo permiten, la extensión PECL puede ser otra forma eficiente de trabajar con YAML. Primero, asegúrate de que la extensión esté instalada:

```bash
pecl install yaml
```

Luego, actívala en tu configuración de `php.ini`:

```ini
extension=yaml.so
```

Aquí te mostramos cómo analizar y emitir YAML:

```php
<?php

// Analizando YAML
$yamlString = <<<YAML
greet: Hola, Mundo!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Creando YAML a partir de un array
$array = [
    'greet' => '¡Hola, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

La salida será similar a la del componente de Symfony, ilustrando el papel de YAML como un puente entre formato legible por humanos y estructuras de array de PHP, facilitando la configuración y manejo de datos de manera más sencilla.
