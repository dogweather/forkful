---
title:                "Eliminando comillas de una cadena"
date:                  2024-01-26T03:37:24.655752-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eliminando comillas de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Eliminar las comillas de una cadena implica quitar las marcas de comillas que encierran la cadena. Los programadores a menudo quieren hacer esto para sanear los datos de entrada, preparar los datos para comparaciones, o adherirse a un formato de datos específico al interactuar con otros programas o sistemas.

## Cómo hacerlo:
Bash tiene varias formas de eliminar comillas de las cadenas. Aquí hay algunos ejemplos rápidos:

```Bash
#!/bin/bash

# Usando la sustitución de variables para eliminar tanto comillas simples como dobles
STRING="\"¡Hola, Mundo!\""
echo ${STRING//\"}

# Usando `tr` para eliminar comillas
STRING="'¡Hola, Mundo!'"
echo $STRING | tr -d "\'"

# Usando `sed` para eliminar comillas
STRING="\"¡Hola, Mundo!\""
echo $STRING | sed 's/"//g'
```

Salida de muestra:

```
¡Hola, Mundo!
¡Hola, Mundo!
¡Hola, Mundo!
```

## Estudio Profundo
Desde hace tiempo, comandos Unix como `tr` y `sed` fueron las herramientas primordiales para el procesamiento de texto. Todavía se utilizan hoy en día por su flexibilidad y potencia en el manejo de transformaciones de texto, como eliminar comillas. Son un elemento básico en el kit de herramientas de cualquier programador de shell.

Bash en sí ha evolucionado desde entonces y la sustitución de variables añade otra capa de simplicidad para manipulaciones de cadenas a pequeña escala. Te ahorra tener que utilizar binarios externos, haciendo tus scripts un poco más eficientes.

Mientras que `tr` es genial para eliminar caracteres, no maneja patrones más complejos. `sed`, por otro lado, utiliza expresiones regulares, por lo que a veces es excesivo y podría ser más lento para operaciones simples.

La elección entre estos métodos depende de tu caso específico. Si necesitas quitar una variedad de comillas y ya estás en el contexto de un script de Bash, usar la sustitución de variables es una opción obvia por su simplicidad. Pero si estás transformando flujos de texto o datos de varias líneas, `tr` y `sed` son tus amigos ideales.

## Ver También:
- El manual de GNU Bash, especialmente las secciones de Expansión de Parámetros y Expansión de Parámetros de Shell: https://www.gnu.org/software/bash/manual/
- El manual del comando `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- La visión general del editor de flujos `sed`: https://www.gnu.org/software/sed/manual/sed.html