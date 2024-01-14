---
title:    "Gleam: Utilizando expresiones regulares"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Por qué utilizar expresiones regulares en Gleam

Si eres un programador en Gleam, seguramente ya has escuchado sobre el uso de expresiones regulares. Pero, ¿por qué deberías utilizarlas en tus proyectos?

Las expresiones regulares son una herramienta poderosa para buscar y manipular texto de forma eficiente. Son especialmente útiles cuando se trabaja con grandes cantidades de datos o cuando se necesita validar entradas del usuario. Además, su uso es ampliamente conocido en el mundo de la programación, por lo que es una habilidad valiosa para cualquier desarrollador.

# Cómo utilizar expresiones regulares en Gleam

En Gleam, el módulo standard `regex` proporciona funciones para trabajar con expresiones regulares. Para comenzar, primero debes importar el módulo en tu archivo:

```Gleam
import regex
```

Luego, puedes utilizar la función `match` para buscar una coincidencia en una cadena de texto utilizando una expresión regular:

```Gleam
let pattern = regex.compile("\\w+")

let result = regex.match(pattern, "Hola Mundo!")

// El resultado será {ok: true, value: "Hola"}
```

En este ejemplo, utilizamos la expresión regular `\w+` para buscar una o más letras, números o guiones bajos en la cadena "Hola Mundo!". El resultado será un registro que indica si se encontró una coincidencia (`ok`) y el valor de la primera coincidencia encontrada (`value`).

# Profundizando en el uso de expresiones regulares

Las expresiones regulares pueden ser más complejas que simplemente buscar una palabra en una cadena. Pueden ser utilizadas para realizar reemplazos, extraer información específica y validar patrones más complejos.

Además, en Gleam también es posible utilizar "capturas" en las expresiones regulares, lo que permite almacenar partes específicas de una coincidencia para su posterior uso en el código.

Para aprender más sobre las diferentes posibilidades y sintaxis de expresiones regulares en Gleam, consulta la documentación oficial del módulo `regex`.

# Ver también

- Documentación oficial del módulo `regex`: https://gleam.run/modules/regex.html
- Ejemplos de expresiones regulares en Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/regex/regex.gleam