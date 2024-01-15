---
title:                "Utilizando expresiones regulares"
html_title:           "Javascript: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué usar expresiones regulares?

Las expresiones regulares son una herramienta poderosa en cualquier lenguaje de programación, incluyendo Javascript. Con ellas, podemos validar y manipular texto de manera fácil y eficiente. Si deseas mejorar en tus habilidades de programación y hacer tu código más robusto, aprender a usar expresiones regulares es una excelente opción.

## Cómo hacerlo?

Para comenzar a utilizar expresiones regulares en Javascript, primero debemos crear un objeto de expresión regular usando la sintaxis ```var regex = new RegExp("patrón", "opciones");```. El patrón es la expresión que deseamos buscar en el texto y las opciones son los distintos parámetros que podemos usar para hacer la búsqueda.

Una vez tengamos nuestro objeto de expresión regular, podemos utilizar los métodos nativos de Javascript para trabajar con él. Estos incluyen ```test()``` para buscar coincidencias y devolver un valor booleano, ```exec()``` para obtener información sobre la coincidencia encontrada y ```replace()``` para reemplazar el texto que cumpla con el patrón.

Veamos un ejemplo de cómo utilizar expresiones regulares para validar una dirección de correo electrónico y obtener la extensión del dominio:

```Javascript
let email = "example@mail.com";
let regex = new RegExp(".+@(.+).com");
regex.test(email); // devuelve true
let extension = regex.exec(email)[1]; // devuelve "mail"
```

## Profundizando más

Además de los métodos básicos mencionados anteriormente, Javascript cuenta con una amplia gama de funciones y métodos específicos para trabajar con expresiones regulares. Por ejemplo, la función ```match()``` nos permite buscar todas las coincidencias en un texto y devolverlas en un array. La función ```search()``` devuelve la posición de la primera coincidencia encontrada en el texto. Y el método ```split()``` divide un string en un array utilizando una expresión regular como separador.

Para aprender más sobre todas las posibilidades que ofrecen las expresiones regulares en Javascript, te recomendamos consultar la documentación oficial de Mozilla o buscar tutoriales y ejercicios interactivos en línea.

## Ver también
- [Documentación oficial de expresiones regulares en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Expresiones regulares en W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Tutorial interactivo de expresiones regulares en Codecademy](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-regex)