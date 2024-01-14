---
title:                "Javascript: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# ¿Por qué borrar caracteres que coinciden con un patrón en Javascript?

Borrar caracteres que coinciden con un patrón en Javascript es una técnica útil para limpiar cadenas de texto y obtener solo la información necesaria. Esto puede ser especialmente útil al manipular datos de formularios o al extraer datos de archivos de texto.

## Cómo hacerlo:

Para borrar caracteres que coinciden con un patrón en Javascript, podemos utilizar el método `.replace()` junto con expresiones regulares.

Por ejemplo, supongamos que queremos eliminar todas las vocales de una cadena de texto. Podríamos hacerlo de la siguiente manera:

```Javascript
let cadena = "Hola mundo";
let resultado = cadena.replace(/[aeiou]/gi, ""); 
console.log(resultado); // Hl mnd
```

En este código, estamos utilizando la expresión regular `/[aeiou]/gi` para indicar que queremos reemplazar todas las vocales (minúsculas y mayúsculas) en la cadena. Luego, cambiamos el patrón de coincidencia por una cadena vacía `""`, lo que resulta en la eliminación de todas las vocales.

Hay muchas otras formas de utilizar expresiones regulares para borrar caracteres que coinciden con un patrón en Javascript. Se pueden combinar diferentes patrones y métodos para obtener resultados específicos según nuestras necesidades.

## Profundizando:

Además de usar expresiones regulares, también podemos utilizar métodos como `.slice()` o `.substring()` para eliminar caracteres de una cadena en Javascript.

Por ejemplo, si queremos borrar los primeros tres caracteres de una cadena, podemos hacerlo de la siguiente manera:

```Javascript
let cadena = "Hola mundo";
let resultado = cadena.slice(3); 
console.log(resultado); // a mundo
```

En este ejemplo, el método `.slice()` nos permite especificar el índice a partir del cual queremos eliminar caracteres. También podemos utilizar el método `.substring()` de manera similar para lograr el mismo resultado.

## Consulta también:

Para más información sobre expresiones regulares y métodos para manipular cadenas en Javascript, puedes consultar los siguientes recursos:

- [Expresiones regulares en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Métodos para manipular cadenas en Javascript](https://www.w3schools.com/js/js_string_methods.asp)