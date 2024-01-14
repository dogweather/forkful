---
title:                "Javascript: Uniendo cadenas de texto"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por qué concatenar cadenas de texto en Javascript

Si eres nuevo en la programación en Javascript, es posible que te preguntes por qué necesitas concatenar cadenas de texto. La respuesta es simple: para combinar diferentes cadenas y crear una cadena más larga y compleja. Esto es especialmente útil cuando trabajas con datos dinámicos o cuando necesitas formatear una salida específica.

## Cómo hacerlo
Para concatenar cadenas de texto en Javascript, puedes utilizar el operador de suma `+` o el método `concat()`. Veamos un ejemplo de cada uno:

```Javascript
// Utilizando el operador +

let nombre = "María";
let apellido = "González";
let nombreCompleto = nombre + " " + apellido;

console.log(nombreCompleto);
//output: María González
```

```Javascript
// Utilizando el método concat()

let ciudad = "Madrid";
let país = "España";
let ubicación = ciudad.concat(", ", país);

console.log(ubicación);
//output: Madrid, España
```

En el primer ejemplo, estamos concatenando dos variables para crear una cadena con el nombre completo de una persona. Nota cómo utilizamos el operador `+` para unir las diferentes cadenas y también agregamos un espacio en blanco entre ellas para que la salida se vea correctamente formateada.

En el segundo ejemplo, estamos usando el método `concat()` para concatenar dos cadenas con una coma y un espacio en el medio. Este método es especialmente útil cuando necesitas agregar diferentes elementos a una cadena, ya que puedes pasar varios argumentos separados por comas.

## Profundizando un poco más
Si bien el operador `+` y el método `concat()` son las formas más comunes de concatenar cadenas de texto en Javascript, también existen otras opciones, como el método `join()` o incluso el uso de plantillas de cadenas. Además, es importante saber que Javascript es un lenguaje de tipado dinámico, lo que significa que también puedes concatenar cadenas y otros tipos de datos, como números o booleanos.

Otro aspecto a tener en cuenta es que, en algunas situaciones, concatenar cadenas puede no ser la mejor opción. Por ejemplo, si trabajas con grandes cantidades de datos, puede ser más eficiente utilizar un `array` y luego unir sus elementos con el método `join()`. Es importante tener en cuenta el rendimiento y la legibilidad de tu código al decidir cómo manejar tus cadenas de texto.

## Ver también
- [Documentación de MDN para concatenar cadenas en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Artículo sobre plantillas de cadenas en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/template_strings)