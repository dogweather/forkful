---
title:    "Gleam: Utilizando expresiones regulares"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Gleam

Las expresiones regulares son una herramienta poderosa para manipular cadenas de texto en cualquier lenguaje de programación, y en Gleam no es la excepción. Con ellas, puedes realizar búsquedas, reemplazos y validaciones de forma eficiente y elegante. Sigue leyendo para descubrir cómo utilizarlas en tus proyectos de Gleam.

## Cómo utilizar expresiones regulares en Gleam

Para utilizar expresiones regulares en Gleam, primero debes importar el módulo `gleam/regex` en tu archivo:

```
import gleam/regex
```

Luego, puedes crear una nueva expresión regular utilizando el constructor `Regex.new` y pasando como argumento el patrón que deseas buscar. Por ejemplo, si quieres buscar todas las palabras que empiecen con la letra "g" en una cadena, puedes hacer lo siguiente:

```
let regex = Regex.new("g[a-z]+")
```

Ahora, puedes utilizar la función `Regex.match` para buscar coincidencias entre tu expresión regular y una cadena de texto. Por ejemplo:

```
let input = "gleam is great"

let result = Regex.match(regex, input)
```

El resultado será una lista de coincidencias, donde cada elemento es una tupla con la posición de inicio y fin de la coincidencia, así como el texto encontrado. En el ejemplo anterior, el resultado será `[(0, 4, "gleam"), (12, 17, "great")]`, ya que ambas palabras empiezan con "g" y contienen solo letras minúsculas.

También puedes utilizar expresiones regulares en conjunto con la función `Regex.replace` para reemplazar partes de una cadena de texto. Por ejemplo:

```
let input = "Greetings from Gleam!"

let modified = Regex.replace(regex, input, "cheerio")

// El resultado será "cheerio from cheerio!"
```

## Profundizando en el uso de expresiones regulares en Gleam

Las expresiones regulares en Gleam son compatibles con muchos de los patrones y operadores habituales en otros lenguajes. Por ejemplo, puedes utilizar los siguientes símbolos para hacer coincidir diferentes tipos de caracteres:

- `.`: Cualquier carácter excepto nueva línea
- `*`: Cero o más ocurrencias del carácter previo
- `+`: Una o más ocurrencias del carácter previo
- `[]`: Cualquier carácter dentro de los corchetes
- `[^]`: Cualquier carácter que NO esté dentro de los corchetes
- `^`: Coincidir al inicio de la cadena
- `$`: Coincidir al final de la cadena

Además, también puedes utilizar modificadores para realizar búsquedas más precisas:

- `i`: Ignora mayúsculas y minúsculas
- `m`: Tratar la cadena como varias líneas
- `g`: Buscar todas las coincidencias, no solo la primera

Si deseas conocer más sobre expresiones regulares y su uso en Gleam, puedes consultar la documentación oficial o explorar las diferentes funciones disponibles en el módulo `gleam/regex`.

## Ver también

- [Documentación de expresiones regulares en Gleam](https://gleam.run/book/tutorials/regular_expressions.html)
- [Módulo Regex en la referencia del lenguaje Gleam](https://gleam.run/stdlib/regex.html)