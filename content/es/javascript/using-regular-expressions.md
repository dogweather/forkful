---
title:                "Javascript: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué usar expresiones regulares

Las expresiones regulares son una herramienta muy útil para programar en Javascript ya que permiten buscar y manipular cadenas de texto de una manera muy precisa. Con regular expressions, puedes ahorrar tiempo y esfuerzo al realizar tareas como validar formularios, extraer información específica de un texto o buscar patrones dentro de una cadena. ¡Incluso puedes utilizarlas para hacer web scraping! En esta guía te explicaremos cómo utilizarlas paso a paso.

## Cómo utilizar expresiones regulares

Las expresiones regulares en Javascript se escriben entre dos barras inclinadas, por ejemplo: ```/patrón/```. Puedes utilizar diferentes patrones y métodos para buscar, reemplazar o extraer partes de una cadena. A continuación, te mostraremos algunos ejemplos de código:

- **Buscar una palabra en una cadena de texto**

```Javascript
let texto = "Las expresiones regulares son muy útiles para programar en Javascript";
let patron = /expresiones regulares/;
console.log(patron.test(texto)); // Output: true
```

- **Extraer un número de una cadena de texto**

```Javascript
let texto = "El número de teléfono de María es 555-123-4567";
let patron = /\d{3}-\d{3}-\d{4}/;
console.log(texto.match(patron)); // Output: 555-123-4567
```

- **Reemplazar una palabra en una cadena de texto**

```Javascript
let texto = "Javascript es un lenguaje de programación muy popular";
let patron = /Javascript/;
console.log(texto.replace(patron, "Python")); // Output: Python es un lenguaje de programación muy popular
```

## Profundizando en el uso de expresiones regulares

Existen diferentes patrones y métodos que puedes utilizar en tu código para aprovechar al máximo las expresiones regulares. Algunos de ellos son:

- **Método ```test()```**: Este método verifica si un patrón se encuentra en una cadena de texto y devuelve un valor booleano (true o false).
- **Método ```match()```**: Devuelve todas las coincidencias de un patrón en una cadena de texto.
- **Método ```replace()```**: Reemplaza una parte de una cadena de texto que coincide con un patrón por otra cadena o valor.
- **Métodos de búsqueda avanzada**: Puedes utilizar símbolos especiales y cuantificadores para buscar patrones más complejos, como por ejemplo, buscar una o más letras consecutivas, buscar una cantidad específica de dígitos o buscar patrones en una posición específica dentro de una cadena.

Con un poco de práctica y conocimiento de los diferentes métodos y patrones disponibles, podrás utilizar expresiones regulares de manera eficiente en tus proyectos de Javascript.

## Ver también

- [Documentación de expresiones regulares en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Tutorial de expresiones regulares en Javascript](https://www.w3schools.com/js/js_regexp.asp)
- [Ejemplos prácticos de expresiones regulares en Javascript](https://www.freecodecamp.org/news/the-30-minute-regex-tutorial-in-es6/)

¡Esperamos que esta guía te haya ayudado a entender mejor cómo utilizar expresiones regulares en tus proyectos de Javascript! Recuerda que la práctica hace al maestro, así que no dudes en seguir explorando y experimentando con esta poderosa herramienta. ¡Hasta la próxima!