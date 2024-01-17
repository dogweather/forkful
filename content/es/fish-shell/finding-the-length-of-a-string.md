---
title:                "Encontrando la longitud de una cadena"
html_title:           "Fish Shell: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En programación, encontrar la longitud de una cadena de caracteres (string) es simplemente contar cuántos caracteres tiene esa cadena. Los programadores a menudo necesitan hacer esto para realizar operaciones y manipulaciones en las cadenas de texto, como cortar o concatenar partes específicas. 

## ¡Cómo hacerlo!

El Fish Shell tiene una función incorporada llamada "string length" que nos permite encontrar la longitud de una cadena. Aquí hay un ejemplo con una cadena de prueba llamada "Hola Mundo":

```
Fish Shell> string length "Hola Mundo"
11
```

Como puedes ver, la cadena tiene 11 caracteres, incluyendo el espacio entre las palabras. También puedes usar esta función con variables que contengan cadenas de caracteres:

```
Fish Shell> set saludo "¡Hola a todos!"
Fish Shell> string length $saludo
14
```

## Profundizando

En la historia de la programación, encontrar la longitud de una cadena de caracteres fue una tarea tediosa y propensa a errores. Los programadores tenían que contar manualmente cada caracter o escribir códigos complejos para resolver el problema. Sin embargo, hoy en día, con la ayuda de herramientas como el Fish Shell, esta tarea se ha vuelto mucho más sencilla y eficiente.

Otra forma de encontrar la longitud de una cadena de caracteres es usando el comando "wc" (word count) en la línea de comandos del sistema operativo. Sin embargo, esto solo funciona si hay una sola palabra en la cadena, de lo contrario, se contarán todas las palabras.

En términos de implementación, la función "string length" del Fish Shell utiliza la función "strlen" de C para encontrar la longitud de una cadena. Esta función cuenta cada caracter en una cadena hasta llegar al final indicado por el caracter nulo '\0'. 

## Ver también

- [Fish Shell: Functions](https://fishshell.com/docs/current/index.html#functions)
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Using the "wc" command](https://www.computerhope.com/unix/uwc.htm)