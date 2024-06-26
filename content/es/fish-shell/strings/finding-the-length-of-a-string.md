---
date: 2024-01-20 17:47:26.335499-07:00
description: "C\xF3mo Hacerlo: Para obtener la longitud de una cadena en Fish, emplea\
  \ la funci\xF3n `string length`. Aqu\xED tienes un par de ejemplos."
lastmod: '2024-03-13T22:44:59.491025-06:00'
model: gpt-4-1106-preview
summary: "Para obtener la longitud de una cadena en Fish, emplea la funci\xF3n `string\
  \ length`."
title: Calculando la longitud de una cadena
weight: 7
---

## Cómo Hacerlo:
Para obtener la longitud de una cadena en Fish, emplea la función `string length`. Aquí tienes un par de ejemplos:

```Fish Shell
# Obtiene la longitud de la cadena "hola"
echo "hola" | string length
```
Salida: `4`

```Fish Shell
# Obtiene la longitud de una cadena almacenada en una variable
set my_string "programación"
string length $my_string
```
Salida: `12`

## Profundización
En Fish, la función `string length` es directa y eficaz, sin las complicaciones de otros shells. Históricamente, otras herramientas como `expr` o `wc` eran comunes antes de tener funciones dedicadas en shells modernos. Comparando con Bash que usa `${#variable}`, Fish ofrece una sintaxis amigable y coherente mediante funciones.

Alternativas en Fish podrían incluir escribir tu propio script para contar caracteres, pero rara vez es necesario. `string length` está optimizado y debería ser tu elección predeterminada.

Detalles de implementación: `string length` maneja caracteres Unicode correctamente, es decir, no solo cuenta bytes, lo que es crucial para la manipulación adecuada de textos multilingües.

## Ver También
Para aprender más sobre los comandos de string en Fish y ver más ejemplos que puedas necesitar, consulta la documentación oficial:

- [Documentación de `string`](https://fishshell.com/docs/current/cmds/string.html)
- [Tutorial de scripting en Fish](https://fishshell.com/docs/current/tutorial.html)

Y para una comprensión más profunda de cómo Fish maneja las cadenas y otros datos, échale un vistazo a:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
