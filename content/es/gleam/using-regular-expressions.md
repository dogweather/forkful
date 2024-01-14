---
title:                "Gleam: Utilizando expresiones regulares"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías utilizar expresiones regulares en Gleam?

Las expresiones regulares son una herramienta poderosa para trabajar con patrones de texto en Gleam. Pueden ayudarte a buscar y manipular información de manera eficiente en tus programas. Si quieres mejorar tus habilidades de programación en Gleam, aprender a utilizar expresiones regulares es una necesidad.

## Cómo utilizar expresiones regulares en Gleam

Para utilizar expresiones regulares en Gleam, primero necesitas importar el módulo "re". Luego, puedes usar la función "re.match()" para encontrar coincidencias en una cadena de texto.

```Gleam
import re

let texto = "¡Hola, mi nombre es Juan!"
let patron = "mi nombre es ([A-Za-z]+)!"
let resultado = re.match(patron, texto)
```

En este ejemplo, estamos buscando la palabra "Juan" en la cadena de texto utilizando una expresión regular. El resultado que obtenemos es un objeto que contiene la palabra "Juan" como una coincidencia. Puedes encontrar más información sobre cómo utilizar expresiones regulares en la documentación de Gleam.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en Gleam te permiten realizar búsquedas más avanzadas en tus cadenas de texto, incluyendo coincidencias de patrones específicos, repeticiones y más. Además, puedes utilizar expresiones regulares en combinación con otras funciones de Gleam para realizar tareas más complejas, como la validación de datos o la manipulación de texto.

En la documentación de Gleam, puedes encontrar una lista completa de las funciones disponibles para trabajar con expresiones regulares, así como ejemplos de sus aplicaciones.

## Ver también

¡Empieza a utilizar expresiones regulares en tus programas de Gleam hoy mismo y aprovecha su potencial para mejorar tu flujo de trabajo y tus habilidades de programación! A continuación, te dejamos algunos recursos que pueden ayudarte a profundizar en el tema:

- [Documentación oficial de Gleam sobre expresiones regulares](https://gleam.run/docs/stdlib/regex.html)
- [Ejemplos prácticos de uso de expresiones regulares en Gleam](https://medium.com/@gleamlang/5-simple-regular-expressions-in-gleam-a3e11d16465a)
- [Tutorial interactivo para aprender expresiones regulares en Gleam](https://learnxinyminutes.com/docs/gleam/)