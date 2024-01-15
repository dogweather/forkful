---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Elixir: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Si estás escribiendo código en Elixir, es posible que en algún momento necesites convertir una cadena de texto a minúsculas. Ya sea para procesar datos, realizar comparaciones o simplemente para estandarizar la forma en que se presentan tus cadenas, saber cómo hacerlo puede ser muy útil.

## Cómo hacerlo

La conversión de una cadena de texto a minúsculas en Elixir es muy sencilla. Puedes utilizar la función `String.downcase/1` y pasarle la cadena de texto que deseas convertir. Veamos un ejemplo:

```Elixir
String.downcase("ELIXIR")
```

El resultado de esta operación sería `"elixir"`, con todas las letras en minúsculas. También puedes utilizar esta función con variables, por ejemplo:

```Elixir
str = "Elixir"
String.downcase(str)
```

De nuevo, el resultado sería `"elixir"`. Además, si la cadena de texto ya está en minúsculas, la función simplemente la devolverá sin cambios.

## Profundizando

La función que utilizamos para convertir las cadenas de texto a minúsculas, `String.downcase/1`, es parte del módulo `String`. Este módulo tiene otras funciones útiles para el manejo de cadenas, como `String.upcase/1` para convertir a mayúsculas, `String.capitalize/1` para capitalizar la primera letra y `String.trim/1` para eliminar espacios en blanco al principio y al final de una cadena, por ejemplo.

También es importante mencionar que en Elixir, las cadenas de texto se representan con el tipo de datos `binary` (binario), lo que significa que se pueden utilizar todas las funciones disponibles para este tipo de datos. Esto incluye funciones para buscar, reemplazar, dividir y unir cadenas, lo que puede ser especialmente útil en aplicaciones de procesamiento de texto.

## Ver también

- [Documentación oficial de Elixir sobre el módulo String](https://hexdocs.pm/elixir/String.html)
- [Artículo sobre tipos de datos en Elixir](https://medium.com/@redguitar/types-in-elixir-9ae13cd89b3d)

¡Ahora tienes la habilidad de convertir cadenas de texto a minúsculas en Elixir! Recuerda siempre revisar la documentación oficial y experimentar con diferentes funciones para conocer todas las herramientas disponibles. ¡Feliz codificación!