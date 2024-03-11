---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:13.861291-07:00
description: "Capitalizar una cadena significa modificarla de modo que la primera\
  \ letra est\xE9 en may\xFAscula y el resto de la cadena en min\xFAscula. Esta es\
  \ una tarea com\xFAn\u2026"
lastmod: '2024-03-11T00:14:33.317844-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena significa modificarla de modo que la primera letra\
  \ est\xE9 en may\xFAscula y el resto de la cadena en min\xFAscula. Esta es una tarea\
  \ com\xFAn\u2026"
title: Capitalizando una cadena de texto
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Capitalizar una cadena significa modificarla de modo que la primera letra esté en mayúscula y el resto de la cadena en minúscula. Esta es una tarea común en el procesamiento de textos, la normalización de entradas de usuario y el formateo de datos para asegurar consistencia o cumplir con criterios de formateo específicos.

## Cómo hacerlo:

En Fish Shell, las cadenas pueden ser manipuladas directamente con funciones integradas, sin la necesidad de herramientas o bibliotecas externas. Para capitalizar una cadena, puedes combinar el comando `string` con subcomandos.

```fish
# Cadena de ejemplo
set cadena_muestra "hello world"

# Capitalizar la primera letra
set cadena_capitalizada (string sub -l 1 -- $cadena_muestra | string upper)(string sub -s 2 -- $cadena_muestra)

echo $cadena_capitalizada
```

Salida:
```
Hello world
```

Para escenarios que requieren la capitalización de múltiples palabras en una cadena (por ejemplo, convertir "hello world" a "Hello World"), iterarías sobre cada palabra, aplicando la lógica de capitalización a cada una:

```fish
# Oración de ejemplo
set oracion "hello fish shell programming"

# Capitalizar cada palabra
set palabras_capitalizadas (string split " " -- $oracion | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Unir las palabras capitalizadas
set oracion_capitalizada (string join " " -- $palabras_capitalizadas)

echo $oracion_capitalizada
```

Salida:
```
Hello Fish Shell Programming
```

Nota que Fish Shell no ofrece directamente un enfoque de comandos único para la capitalización completa de oraciones de la misma manera en que algunos lenguajes de programación lo hacen con sus métodos de cadena. Por lo tanto, combinar `string split`, `string sub`, `string upper` y luego reunir representa un enfoque idiomático en Fish Shell para lograr esto.
