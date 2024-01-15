---
title:                "Extrayendo subcadenas"
html_title:           "Fish Shell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con un texto largo y desordenado y solo necesitas extraer una parte específica de él? Bueno, con Fish Shell puedes hacer precisamente eso. En este artículo, te explicaré cómo extraer subcadenas de una manera rápida y sencilla.

## Cómo

¡Es hora de poner tus habilidades de programación en acción! Sigue estos pasos para extraer subcadenas en Fish Shell:

1. Primero, necesitas tener una cadena de texto de la cual quieras extraer una parte. Puedes asignar una cadena a una variable utilizando el operador `set`, por ejemplo: 

```Fish Shell
set text "Hola mundo"
```

2. Después de tener tu cadena, debes indicar qué parte quieres extraer utilizando los índices. Los índices en Fish Shell comienzan en `1` y se colocan entre corchetes después de la variable que contiene la cadena de texto. Por ejemplo, si queremos extraer la palabra "mundo" en nuestro ejemplo anterior, utilizaremos `[6-10]` para indicar los índices.

```Fish Shell
echo $text[6-10]
```

3. También puedes extraer subcadenas utilizando el comando `string sub` y proporcionando la cadena, el índice inicial y la longitud de la subcadena que deseas extraer.

```Fish Shell
echo (string sub $text 6 5)
```

¡Felicidades! Has aprendido a extraer subcadenas en Fish Shell. Ahora puedes utilizar esta técnica para manipular y organizar tus cadenas de texto de manera eficiente.

## Deep Dive

Ahora, profundicemos un poco más en la extracción de subcadenas. En Fish Shell, los índices pueden ser expresiones matemáticas que se evalúan dinámicamente. Esto significa que puedes utilizar variables y operaciones matemáticas para definir tus índices.

Además, también puedes utilizar un signo `^` para indicar el comienzo de la cadena en lugar de indicar un índice específico. Por ejemplo, si queremos extraer la palabra "mundo" en nuestro ejemplo anterior, también podríamos utilizar `[^6-]` para indicar que queremos comenzar a extraer a partir del sexto carácter hasta el final de la cadena.

## Ver también

- [Tutorial de Fish Shell](https://fishshell.com/docs/2.7/)

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/2.7/index.html)