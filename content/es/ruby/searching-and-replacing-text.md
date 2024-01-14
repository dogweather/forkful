---
title:                "Ruby: Buscando y reemplazando texto"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en la situación en la que tienes un gran bloque de texto y necesitas reemplazar una palabra o frase en específico en todo el texto? Puede ser un proceso tedioso y propenso a errores si lo haces manualmente. Aquí es donde entra en juego la búsqueda y reemplazo de texto en Ruby.

## Cómo hacerlo

Ruby proporciona una forma fácil y eficiente de buscar y reemplazar texto utilizando el método `gsub`. Este método toma dos argumentos: el patrón que quieres buscar y el texto con el que deseas reemplazarlo.

Veamos un ejemplo en el que queremos reemplazar la palabra "perro" por la palabra "gato" en el siguiente texto:

```
string = "Tengo un perro llamado Max."
```

Usamos `gsub` para reemplazar "perro" por "gato":

```
string.gsub("perro", "gato")
```

El resultado será: "Tengo un gato llamado Max."

También puedes ser más específico en tu búsqueda utilizando expresiones regulares. Por ejemplo, si queremos reemplazar todas las palabras que comiencen con "g" en el siguiente texto:

```
string = "La granja del granjero Greg es muy grande."
```

Podemos hacerlo con la siguiente expresión regular:

```
string.gsub(/\bg\w+/, "casa")
```

El resultado será: "La casa del casero César es muy grande."

## Profundizando

El método `gsub` es muy poderoso ya que nos permite realizar búsquedas y reemplazos a nivel de patrones en lugar de solo palabras específicas. Puedes utilizar expresiones regulares para realizar búsquedas más complejas, reemplazar patrones específicos o incluso hacer uso de la sintaxis de bloques para un control más preciso sobre la manipulación del texto.

Además, Ruby también cuenta con otros métodos útiles para la manipulación de cadenas de texto, como `sub` (para reemplazar solo la primera aparición), `scan` (para encontrar todas las apariciones de un patrón) y `match` (para obtener información sobre un patrón específico).

En resumen, la búsqueda y reemplazo de texto en Ruby es una herramienta imprescindible para cualquier desarrollador que trabaje con cadenas de texto y puede ahorrarte mucho tiempo y esfuerzo en tus proyectos.

## Ver también

- [Documentación de `gsub` en Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Manipulación de cadenas de texto en Ruby](https://www.rubyguides.com/ruby-tutorial-string/)

Gracias por leer y ¡feliz codificación!