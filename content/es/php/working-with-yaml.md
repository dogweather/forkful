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

## Por qué
Si eres un programador PHP, probablemente hayas escuchado hablar de YAML, un formato de serialización de datos. Trabajar con YAML puede ayudarte a organizar y gestionar grandes cantidades de datos de una manera más legible y estructurada. Además, es ampliamente compatible con otros lenguajes de programación.

## Cómo hacerlo
Para empezar a trabajar con YAML en PHP, debes asegurarte de tener instalada la extensión YAML en tu entorno de desarrollo. Una vez instalada, puedes utilizar la función `yaml_parse()` para convertir un archivo YAML en un array PHP. Por ejemplo:
```PHP
$datos = yaml_parse(file_get_contents("datos.yml"));
print_r($datos);
```
Este código leerá el archivo "datos.yml" y lo convertirá en un array en PHP, que luego puede ser tratado y manipulado de la misma manera que cualquier otro array en el lenguaje.

## Profundizando
Aunque la función `yaml_parse()` es útil para la mayoría de las situaciones, puede que necesites un mayor control sobre el proceso de parseo de YAML. En ese caso, puedes utilizar la clase `Yaml` del paquete Symfony para realizar tareas más avanzadas, como validar la sintaxis o convertir objetos a YAML. Por ejemplo:
```PHP
$yaml = new Yaml();
$data = $yaml->parse(file_get_contents("datos.yml"));
$yaml->dump($data, "datos_nuevos.yml");
```
En este ejemplo, utilizamos la función `parse()` para convertir el archivo YAML en un array, y luego la función `dump()` para convertir ese array en un nuevo archivo YAML llamado "datos_nuevos.yml".

## Ver también
- La documentación oficial de la extensión YAML para PHP: https://php.net/manual/es/book.yaml.php
- La documentación de la clase `Yaml` del paquete Symfony: https://symfony.com/doc/current/components/yaml.html