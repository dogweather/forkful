---
title:    "Elixir: Extrayendo subcadenas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

##¿Por qué extraer subcadenas en Elixir?

Extraer subcadenas es una técnica muy útil en la programación en Elixir. Esta funcionalidad nos permite obtener una parte específica de una cadena de texto para poder manipular o utilizarla de manera independiente. En este artículo, aprenderemos por qué es importante saber cómo extraer subcadenas y cómo hacerlo de manera efectiva en Elixir.

## Cómo extraer subcadenas en Elixir

La sintaxis para extraer subcadenas en Elixir es sencilla:

```
Elixir> cadena = "¡Hola Mundo!"
"Hola Mundo!"
```

Podemos utilizar el operador de acceso `[]` seguido de un índice numérico para obtener un carácter específico de la cadena:

```
Elixir> cadena[0]
"¡"
```

También podemos utilizar una lista de índices para obtener múltiples caracteres:

```
Elixir> cadena[1,3]
"ola"
```

Incluso podemos utilizar rangos para obtener una subcadena con ciertos límites:

```
Elixir> cadena[4..8]
"a Mun"
```

## Profundizando en la extracción de subcadenas en Elixir

Además de acceder a caracteres específicos o rangos de una cadena, Elixir también nos ofrece funciones más avanzadas para extraer subcadenas.

La función `String.slice/2` nos permite obtener una subcadena a partir de un índice de inicio y un índice de fin:

```
Elixir> cadena = "¡Hola Mundo!"
"Hola Mundo!"

Elixir> String.slice(cadena, 1, 4)
"ola "
```

También podemos utilizar la función `String.split/3` para dividir una cadena en partes separadas por un determinado carácter:

```
Elixir> cadena = "¡Hola,Mundo!"
"Hola,Mundo!"

Elixir> String.split(cadena, ",")
["¡Hola", "Mundo!"]
```

## Ver también

- [Documentación oficial de Elixir sobre extracción de subcadenas](https://hexdocs.pm/elixir/String.html#slice/2)
- [Ejemplos de extracción de subcadenas en Elixir](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/string.ex)

Esperamos que este artículo te haya ayudado a entender por qué es importante saber cómo extraer subcadenas en Elixir y cómo hacerlo efectivamente. ¡Sigue explorando y mejorando tus habilidades en este lenguaje de programación funcional!