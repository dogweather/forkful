---
title:                "Trabajando con yaml"
html_title:           "Bash: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con YAML?
Si estás buscando una forma sencilla de almacenar y transmitir datos en tus scripts de Bash, entonces YAML es la respuesta. Es un formato de texto legible para humanos y fácil de entender, lo que lo hace ideal para aquellos que no están familiarizados con la programación.

## Cómo hacerlo

Para trabajar con YAML en Bash, necesitarás usar el comando "yq", que es una herramienta de línea de comandos para procesar y manipular archivos YAML. Aquí tienes un ejemplo de cómo puedes utilizarlo para leer y escribir en un archivo YAML:

```
# Leer un archivo YAML
yq r example.yml

# Escribir en un archivo YAML
yq w -i example.yml foo.bar "nuevo valor"
```

Esto te permitirá acceder y modificar los datos almacenados en tu archivo YAML de manera rápida y fácil.

## Profundizando

YAML tiene una serie de características útiles que hacen que sea una gran opción para trabajar con datos en Bash. Por ejemplo, puedes crear listas y diccionarios con sintaxis sencilla y anidarlos para estructurar tus datos de manera más eficiente.

Además, también puedes utilizar comentarios en tus archivos YAML para explicar qué hace cada elemento y asegurarte de que otros entiendan tu código.

Para aprovechar al máximo YAML, también puedes explorar el uso de otras herramientas como "jq" y "yq" para procesar y manipular archivos JSON y YAML juntos.

## Ver también

- [Página oficial de YAML](https://yaml.org/)
- [Documentación de yq](https://mikefarah.gitbook.io/yq/)
- [Introducción a JSON y YAML en Bash](https://www.ostechnix.com/introduction-json-yaml-bash-scripts/)