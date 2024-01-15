---
title:                "Uso de expresiones regulares"
html_title:           "Gleam: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado buscando un patrón específico en grandes cantidades de texto? Ahí es donde entran en juego las expresiones regulares. Con Gleam, puedes utilizar expresiones regulares para buscar y manipular cadenas de texto de manera más eficiente. ¡Sigue leyendo para aprender cómo!

## Cómo hacerlo

```Gleam
let cadena = "¡Hola mundo!"
let regex = /\w+/
regex.match(cadena) == Some("Hola") // Verdadero
```

En el ejemplo anterior, creamos una cadena de texto y una expresión regular que busca la primera palabra en la cadena. Al utilizar el método `match`, podemos verificar si la cadena cumple con el patrón de la expresión regular y obtener la palabra coincidente. 

Las expresiones regulares en Gleam también tienen una variedad de métodos útiles como `replace`, `split` y `search` que te permitirán trabajar con cadenas de texto de manera más eficiente.

## Inmersión profunda

Las expresiones regulares pueden parecer intimidantes al principio, pero una vez que las entiendes, se convertirán en una herramienta invaluable en tu caja de herramientas de programación. Puedes utilizar diferentes metacaracteres para definir patrones más complejos, como `\d` para representar dígitos y `\s` para representar espacios en blanco.

También puedes utilizar cuantificadores para repetir patrones, como `*` para representar 0 o más veces y `+` para representar 1 o más veces. Además, las expresiones regulares también tienen opciones de búsqueda avanzadas, como la búsqueda con sensibilidad a mayúsculas y minúsculas o el uso de grupos para agrupar patrones.

¿Quieres profundizar aún más en el mundo de las expresiones regulares? Echa un vistazo a nuestra documentación oficial de Gleam y empieza a jugar con diferentes patrones y métodos.

## Ver también

- Documentación oficial de Gleam sobre expresiones regulares: https://gleam.run/book/tour/regular-expressions.html
- Ejemplos de expresiones regulares en Gleam: https://github.com/gleam-lang/gleam/tree/master/examples/regular_expressions
- Artículo sobre las ventajas del uso de expresiones regulares en Gleam: https://medium.com/@gleamlang/regular-expressions-in-gleam-65b8acdf8128