---
title:    "Gleam: Buscando y reemplazando texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Cuando se está escribiendo código, puede ser necesario hacer cambios en el texto, ya sea para corregir errores u optimizar el rendimiento. La función de búsqueda y reemplazo de texto en Gleam es una herramienta poderosa para hacer estos cambios de manera rápida y eficiente.

## Cómo hacerlo

La función de búsqueda y reemplazo de texto en Gleam se puede realizar con los métodos `String.replace` y `Regex.replace`. En el primer caso, se puede proporcionar un valor de búsqueda y un valor de reemplazo para el texto deseado. Por ejemplo:

```Gleam
let original_text = "¡Hola, mundo!"
let new_text = String.replace(original_text, "mundo", "Gleam")
```

Esto producirá el texto "¡Hola, Gleam!" como resultado. Además, la función `String.replace` también tiene opciones para realizar varias sustituciones y realizar solo una sustitución en una parte específica del texto.

Para hacer un reemplazo basado en una expresión regular, se puede utilizar el método `Regex.replace`. Por ejemplo:

```Gleam
let original_text = "Número de teléfono: 555-1234"
let new_text = Regex.replace(original_text, "[0-9]+", "1234")
```

Esto producirá el texto "Número de teléfono: 1234". Al igual que con `String.replace`, se pueden proporcionar opciones adicionales para realizar múltiples sustituciones y alinear el reemplazo en una posición específica dentro del texto.

## Profundizando

Las funciones `String.replace` y `Regex.replace` son muy versátiles y pueden usarse para realizar cambios en textos largos y complejos. También se pueden combinar con otras funciones de Gleam, como la función `substring`, para dar aún más control sobre el texto de entrada.

Por ejemplo, si se desea reemplazar solo una parte de una URL en un texto, se puede utilizar `String.replace` junto con `substring` para encontrar y reemplazar el valor deseado. Esta capacidad de realizar cambios precisos y flexibles en el texto hace que la función de búsqueda y reemplazo de texto en Gleam sea una herramienta poderosa para cualquier programador.

## Ver también

- Documentación de búsqueda y reemplazo de texto en Gleam: https://gleam.run/book/std/string.html#replace
- Documentación de expresiones regulares en Gleam: https://gleam.run/book/std/regex.html