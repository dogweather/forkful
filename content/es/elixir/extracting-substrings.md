---
title:                "Elixir: Extrayendo subcadenas"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Elixir?

Extraer subcadenas es una técnica muy útil en la programación de Elixir. Nos permite obtener una porción de una cadena más grande, que puede ser útil en diferentes situaciones, como por ejemplo, si necesitamos procesar una parte específica de un texto o validar un formato determinado. En esta publicación, veremos cómo podemos extraer subcadenas en Elixir y profundizaremos en algunas de sus características.

## Cómo hacerlo

Extraer subcadenas en Elixir es muy sencillo gracias a la función `String.slice/2`. Esta función toma como argumento la cadena de texto y una lista que indica el rango de índices de la subcadena que queremos extraer. Veamos un ejemplo:

```Elixir
texto = "Hola Mundo"
String.slice(texto, 0..3) # devuelve "Hola"
String.slice(texto, 5..9) # devuelve "Mundo"
```

En el primer ejemplo, indicamos que deseamos obtener los caracteres desde el índice 0 hasta el índice 3 de la cadena `texto`, lo que resulta en la subcadena "Hola". En el segundo ejemplo, hacemos lo mismo pero con un rango diferente, por lo que obtenemos "Mundo". Como podemos ver, el índice de inicio del rango es inclusivo, mientras que el índice final es exclusivo.

También podemos utilizar números negativos para indicar el rango. En este caso, los índices se cuentan desde el final de la cadena hacia el principio. Por ejemplo:

```Elixir
texto = "Hola Mundo"
String.slice(texto, -5..-1) # devuelve "Mundo"
```

En este caso, el rango indica que deseamos obtener los últimos 5 caracteres de la cadena `texto`, obteniendo así la subcadena "Mundo". Además, si omitimos el índice final del rango, la función `String.slice/2` devolverá la subcadena desde el índice indicado hasta el final de la cadena. Por ejemplo:

```Elixir
texto = "Hola Mundo"
String.slice(texto, 5..) # devuelve "Mundo"
```

Por último, si queremos obtener un solo carácter, podemos utilizar la función `String.at/2`, especificando únicamente el índice del carácter que deseamos obtener. Por ejemplo:

```Elixir
texto = "Hola Mundo"
String.at(texto, 5) # devuelve "M"
```

## Profundizando en la extracción de subcadenas

Además de la función `String.slice/2`, Elixir también cuenta con otras funciones útiles para extraer subcadenas. Por ejemplo, si necesitamos obtener una subcadena de una cadena basada en una determinada secuencia de caracteres, podemos utilizar `String.split/2`. Esta función toma como argumentos la cadena y el delimitador que queremos utilizar para dividir la cadena. Veamos un ejemplo:

```Elixir
texto = "Hola,Mundo"
String.split(texto, ",") # devuelve ["Hola", "Mundo"]
```

Otra función útil es `String.trim/2`, que nos permite eliminar espacios en blanco al inicio y al final de una cadena. Esto puede ser especialmente útil cuando trabajamos con cadenas de texto que provienen de una fuente externa, como un archivo o una API. Por ejemplo:

```Elixir
texto = "    Hola Mundo    "
String.trim(texto) # devuelve "Hola Mundo"
```

## Ver también

- Documentación oficial de Elixir sobre `String`: https://hexdocs.pm/elixir/String.html
- Tutorial de Elixir en español: https://www.elixirtutorials.com/
- Comunidad de Elixir en español: https://elixir-lang.lat/