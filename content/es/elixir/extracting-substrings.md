---
title:                "Elixir: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Elixir?

Extraer subcadenas es una habilidad esencial para cualquier programador de Elixir, ya que permite manipular y trabajar con cadenas de texto de una manera más eficiente y efectiva. Con la capacidad de extraer subcadenas, puedes obtener partes específicas de una cadena para realizar operaciones como búsqueda, reemplazo y validación.

## ¿Cómo hacerlo?

Extraer subcadenas en Elixir es bastante sencillo con el uso de la función `String.slice/3` y la sintaxis de rangos. Veamos algunos ejemplos prácticos para comprender mejor cómo funciona.

```Elixir
# Extraer una subcadena de una cadena dada
String.slice("Hola mundo", 1..4)
# Output: "Hola"

# Extraer una subcadena a partir de un índice específico hasta el final de la cadena
String.slice("Programando en Elixir", 13..-1)
# Output: "Elixir"

# Extraer una subcadena utilizando una expresión regular
String.slice("¡Hola mundo!", ~r/hola/i)
# Output: "Hola"

# Extraer una subcadena utilizando una expresión regular y especificando la posición inicial
String.slice("¡Hola mundo!", ~r/[a-z]/, pos: 6)
# Output: "l mundo"
```

Como se puede observar en los ejemplos anteriores, la función `String.slice/3` toma tres parámetros: la cadena de texto, el rango de la subcadena que se desea extraer y (opcionalmente) las opciones de configuración. Esta función también permite extraer subcadenas utilizando expresiones regulares para mayor flexibilidad.

## Profundizando

La función `String.slice/3` es solo una de las muchas formas en las que puedes extraer subcadenas en Elixir. También existen otras funciones, como `String.split/3` y `String.replace/4`, que pueden ser útiles para esta tarea.

Además, es importante mencionar que las cadenas en Elixir son inmutables, lo que significa que la función `String.slice/3` no modifica la cadena original, sino que retorna una versión nueva con la subcadena extraída. Esto es beneficioso ya que garantiza la integridad de los datos y previene posibles errores.

## Ver también

- [Documentación oficial de Elixir sobre cadenas de texto](https://hexdocs.pm/elixir/String.html)
- [Artículo sobre la manipulación de cadenas en Elixir](https://medium.com/@Jeyachandran/coding-with-elixir-manipulating-strings-c5ad808920ef)
- [Curso en línea gratuito sobre programación en Elixir (en español)](https://www.udemy.com/course/elixir-elixir-programming-for-beginners/?couponCode=ELIXIR_ES)