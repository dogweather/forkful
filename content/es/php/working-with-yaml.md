---
title:                "Trabajando con yaml"
html_title:           "PHP: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# ¿Qué y Por Qué?

Trabajar con YAML es una forma de almacenar y manejar datos estructurados en formato de texto legible. Los programadores utilizan YAML para facilitar la organización y manipulación de datos, especialmente en aplicaciones web y sistemas de gestión de contenido.

# Cómo:

Para utilizar YAML en PHP, primero debemos asegurarnos de tener la extensión "yaml" instalada. Podemos hacerlo a través del gestor de paquetes PECL o a través de la configuración de PHP. Una vez que tengamos la extensión instalada, podemos utilizar la función `yaml_parse()` para leer un archivo YAML y convertirlo en un array de PHP. Por ejemplo:

```php
<?php
$yaml = "nombre: John
edad: 25
hobbies:
  - programar
  - leer
  - jugar videojuegos";

// convierte el YAML en un array de PHP
$data = yaml_parse($yaml);

// acceder a los datos
echo "El nombre es: " . $data['nombre']; // salida: "El nombre es: John"
echo "La edad es: " . $data['edad']; // salida: "La edad es: 25"
echo "Hobbies: " . implode(", ", $data['hobbies']); // salida: "Hobbies: programar, leer, jugar videojuegos"
```

# Profundizando:

YAML (acrónimo de "YAML Ain't Markup Language") fue creado en 2001 como una alternativa a formatos de datos más complejos como XML y JSON. Se basa en una estructura de clave-valor con indentaciones para representar datos de una manera legible para humanos.

Existen otras alternativas para almacenar y manejar datos estructurados en PHP, como XML, JSON y CSV. Sin embargo, YAML destaca por su facilidad de uso y legibilidad. Además, su integración con Symfony y Laravel lo ha hecho popular entre los desarrolladores de PHP.

En la implementación de YAML en PHP, se utilizan las librerías libyaml o Syck para el parseo y la conversión a arrays de PHP. También es importante mencionar que YAML es un formato de datos seguro, ya que no permite la ejecución de código malicioso.

# Ver también:

- [Documentación oficial de YAML](https://yaml.org/)
- [Extensión "yaml" de PHP](https://www.php.net/manual/es/book.yaml.php)
- [Biblioteca Symfony YAML](https://symfony.com/doc/current/components/yaml.html)
- [Biblioteca Laravel YAML](https://laravel.com/docs/8.x/eloquent-serialization#basic-usage)