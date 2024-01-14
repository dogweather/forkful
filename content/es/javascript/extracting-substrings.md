---
title:    "Javascript: Extrayendo subcadenas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Extraer subcadenas es una habilidad fundamental en la programación que nos permite manipular y trabajar con cadenas de texto de manera eficiente. Ser capaz de extraer una parte específica de una cadena nos permite realizar operaciones más complejas y realizar tareas específicas con mayor precisión.

## Cómo hacerlo
Para extraer subcadenas en Javascript, podemos utilizar el método `substring()` que nos permite especificar la posición inicial y final de la subcadena que deseamos extraer. Por ejemplo:

```Javascript
let cadena = "Hola mundo";
let subcadena = cadena.substring(0,4);
console.log(subcadena); // "Hola"
```

En este ejemplo, estamos extrayendo la subcadena que va desde el primer carácter hasta el cuarto de la variable `cadena` y luego imprimiéndola en la consola.

También podemos utilizar el método `slice()` que funciona de manera similar a `substring()` pero nos permite especificar la posición inicial y también la posición final de la subcadena. Por ejemplo:

```Javascript
let cadena = "¡Hola a todos!";
let subcadena = cadena.slice(0,4);
console.log(subcadena); // "¡Hola"
```

Además, podemos utilizar el método `substr()` que nos permite especificar la posición inicial y la longitud de la subcadena que deseamos extraer. Por ejemplo:

```Javascript
let cadena = "¡Hola a todos!";
let subcadena = cadena.substr(0,4);
console.log(subcadena); // "¡Hol"
```

## Profundizando
Existen numerosas razones por las que podríamos necesitar extraer subcadenas en Javascript. Por ejemplo, podemos utilizar esta habilidad para formatear y manipular nombres y apellidos, extraer información específica de una dirección o correo electrónico, entre otras cosas.

Además, también podemos utilizar expresiones regulares para extraer subcadenas que coincidan con un patrón determinado. Por ejemplo, si quisieramos extraer todos los dígitos de una cadena, podríamos usar la expresión regular `/[0-9]/g` en combinación con el método `match()` de Javascript. Ejemplo:

```Javascript
let cadena = "Hoy es 15 de julio";
let digitos = cadena.match(/[0-9]/g);
console.log(digitos); // ["1", "5"]
```

## Ver también
- [Método substring() en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/substring)
- [Método slice() en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/slice)
- [Método substr() en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/substr)