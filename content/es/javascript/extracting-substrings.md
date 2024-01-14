---
title:    "Javascript: Extrayendo subcadenas"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad importante en la programación, ya que nos permite obtener una parte específica de una cadena de texto. Esto es especialmente útil cuando se trabaja con grandes cantidades de datos y se necesita acceder a cierta información específica dentro de una cadena.

## Cómo hacerlo

La extracción de subcadenas se logra mediante el uso de la función `substring()` en Javascript. Esta función toma dos parámetros: el índice en el que se inicia la extracción y el índice en el que se termina.

Por ejemplo, si tenemos una cadena de texto "Hola Mundo" y queremos extraer la palabra "Mundo", podemos hacerlo de la siguiente manera:

```javascript
let cadena = "Hola Mundo";
let subcadena = cadena.substring(5,10); // Esto nos dará "Mundo"
console.log(subcadena);
```

El primer parámetro indica el índice en el que comienza la extracción, en este caso, la letra "M" está en el índice 5. El segundo parámetro indica el índice en el que termina la extracción, en este caso, la letra "o" está en el índice 10.

Otra forma de extraer subcadenas es utilizando la función `slice()`. Esta función también toma dos parámetros, pero en este caso, el segundo parámetro indica la longitud de la subcadena en lugar del índice en el que termina.

Por ejemplo, si queremos extraer la palabra "Hola":

```javascript
let cadena = "Hola Mundo";
let subcadena = cadena.slice(0,4); // Esto nos dará "Hola"
console.log(subcadena);
```

El primer parámetro indica el índice en el que comienza la extracción, en este caso, la letra "H" está en el índice 0. El segundo parámetro indica la longitud de la subcadena, en este caso, 4 letras.

## Profundizando

Además de la función `substring()` y `slice()`, existen otras formas de extraer subcadenas en Javascript. Podemos utilizar la función `substr()` que toma dos parámetros: el índice en el que comienza la extracción y la longitud de la subcadena.

También podemos combinar estas funciones con métodos como `indexOf()` o `lastIndexOf()` para encontrar el índice en el que comienza o termina la subcadena que queremos extraer.

La extracción de subcadenas es una habilidad importante en la programación y conocer las diferentes formas de hacerlo nos ayudará a mejorar nuestras habilidades de manipulación de cadenas.

## Ver también

- [Documentación de MDN sobre la función `substring()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentación de MDN sobre la función `slice()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Documentación de MDN sobre la función `substr()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substr)