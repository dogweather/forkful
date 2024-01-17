---
title:                "Trabajando con json"
html_title:           "Fish Shell: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

# ¿Qué es y por qué trabajamos con JSON en Fish Shell?

JSON (JavaScript Object Notation) es un formato de datos ligero y de texto que se utiliza en el intercambio de información entre aplicaciones. Los programadores trabajan con JSON porque es fácil de leer y escribir, y es compatible con muchos lenguajes de programación.

# Cómo hacerlo:

Para trabajar con JSON en Fish Shell, primero debes asegurarte de tener instalado el plugin `json` utilizando el administrador de paquetes `fisher`.

```
fisher install json
```

A continuación, puedes utilizar los comandos de la librería `jq` para procesar y manipular datos JSON. Por ejemplo, para obtener los valores de una clave específica en un archivo JSON, puedes usar el comando `json get`.

```
json get mi_archivo.json .clave
```

Esto devolverá el valor de la clave `clave` en el archivo `mi_archivo.json`.

# Profundizando:

JSON fue creado en 2001 por Douglas Crockford y se ha convertido en un formato de datos muy popular en la programación web. Alternativas a JSON incluyen el formato XML, pero JSON es ampliamente utilizado debido a su simplicidad y facilidad de uso.

Fish Shell utiliza la librería `jq` para trabajar con JSON, que proporciona una gran cantidad de funciones y herramientas para procesar y manipular datos JSON en línea de comandos. Puedes encontrar más información sobre `jq` en su sitio web oficial.

# Ver también:

- Página oficial de `jq`: https://stedolan.github.io/jq/
- Página oficial de Fish Shell: https://fishshell.com/