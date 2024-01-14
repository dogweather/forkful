---
title:                "Gleam: Capitalizando una cadena"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

Si eres un desarrollador que trabaja con strings en Gleam, es posible que te hayas preguntado alguna vez cómo capitalizar una cadena de texto. Puede que estés buscando una forma de mejorar la legibilidad de tus datos o simplemente necesites formatear correctamente una salida de texto. Sea cual sea la razón, saber cómo capitalizar una cadena puede ser una herramienta útil en tu caja de herramientas de programación.

## Cómo hacerlo

Para capitalizar una cadena en Gleam, podemos utilizar la función `String.to_title()` que toma una cadena como argumento y devuelve la versión capitalizada de esa cadena.

```Gleam
let nombre = "juan";
let nombre_capitalizado = String.to_title(nombre);
```

La variable `nombre_capitalizado` ahora contiene la cadena "Juan" en lugar de "juan". Esto se debe a que la función `String.to_title()` convierte la primera letra de la cadena en mayúscula y el resto en minúsculas.

También podemos utilizar `String.to_title()` para capitalizar palabras compuestas, como en el siguiente ejemplo:

```Gleam
let nombre_completo = "maria perez";
let nombre_completo_capitalizado = String.to_title(nombre_completo);
```

La variable `nombre_completo_capitalizado` ahora contendrá la cadena "Maria Perez", donde ambas palabras están capitalizadas correctamente.

## Profundizando

Mientras que la función `String.to_title()` es útil para casos simples de capitalización, no es adecuada para manejar todos los casos posibles. Por ejemplo, no funciona correctamente con nombres que contienen apóstrofes o guiones. En estos casos, es posible que necesites implementar tu propia función de capitalización personalizada utilizando patrones y combinadores de Gleam.

También es importante tener en cuenta las diferencias entre los idiomas al capitalizar cadenas. En español, por ejemplo, se utilizan mayúsculas para los nombres de las personas y los lugares, pero no necesariamente en otros contextos. Por lo tanto, es importante tener una comprensión sólida de las reglas de capitalización en el idioma en el que estás trabajando.

## Ver también

- Documentación oficial de la función `String.to_title()`: https://gleam.run/modules/gleam_std.String.html#function.to_title

- Ejemplos prácticos de capitalización de cadenas en diferentes lenguajes de programación: https://www.dotnetperls.com/capitalize

- Cómo manejar casos especiales en la capitalización de cadenas en Gleam: https://medium.com/@thatjonathanchen/write-a-capitalizer-function-in-gleam-e6fd92c3a010