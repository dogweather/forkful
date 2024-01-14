---
title:                "Javascript: Capitalizando una cadena"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena en Javascript

En el mundo de la programación, a menudo nos encontramos con la necesidad de modificar una cadena de texto para que empiece con mayúscula, ya sea para mantener un formato consistente o para fines de presentación. Afortunadamente, en Javascript existe una función específica para esto: `toUpperCase()`. En este artículo, aprenderemos cómo capitalizar una cadena en Javascript y por qué es útil hacerlo.

## Cómo capitalizar una cadena en Javascript

Para capitalizar una cadena en Javascript, podemos utilizar el método `toUpperCase()` junto con la función `charAt()` para obtener el primer carácter de la cadena y convertirlo a mayúscula. Veamos un ejemplo de cómo hacerlo en código:

```Javascript
var cadena = "ejemplo";
var primeraLetra = cadena.charAt(0).toUpperCase();
var restoCadena = cadena.slice(1);
var cadenaCapitalizada = primeraLetra + restoCadena;
console.log(cadenaCapitalizada); // Salida: "Ejemplo"
```

En este ejemplo, utilizamos la función `charAt()` para obtener el primer carácter de la cadena y luego lo convertimos a mayúscula con `toUpperCase()`. Después, utilizamos la función `slice()` para obtener el resto de la cadena y, finalmente, concatenamos ambas partes para obtener la cadena capitalizada.

También podemos utilizar el método `replace()` para capitalizar la primera letra de una cadena de forma más sencilla:

```Javascript
var cadena = "ejemplo";
var cadenaCapitalizada = cadena.replace(cadena.charAt(0), cadena.charAt(0).toUpperCase());
console.log(cadenaCapitalizada); // Salida: "Ejemplo"
```

En este caso, utilizamos `replace()` para reemplazar el primer carácter de la cadena con el mismo carácter pero convertido a mayúscula.

## Profundizando en la capitalización de cadenas en Javascript

Para aquellos que quieran profundizar en el tema, hay varias cosas que tener en cuenta al capitalizar una cadena en Javascript. Por ejemplo, es importante tener en cuenta que la función `toUpperCase()` sólo convierte a mayúscula los caracteres latinos. Si queremos capitalizar letras como la ñ o la ü, debemos utilizar funciones adicionales o bibliotecas externas.

También es importante señalar que, si bien funciona con casi cualquier carácter, la función `toUpperCase()` puede tener problemas con caracteres de otros idiomas o sistemas de escritura, por lo que siempre es recomendable realizar pruebas exhaustivas antes de utilizarla en un proyecto importante.

En cuanto al rendimiento, la función `toUpperCase()` no es la opción más eficiente si vamos a capitalizar una gran cantidad de cadenas, ya que cada vez que la utilizamos estamos creando un nuevo objeto. En este caso, puede ser más conveniente utilizar otros métodos como `replace()` o la función `substr()`.

## Ver también

- [Referencia de strings en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Cómo capitalizar strings en otros idiomas en Javascript](https://medium.com/@maffelu/globalization-or-the-problems-of-i18n-in-javascript-e393c962296c)
- [Biblioteca para capitalizar strings en diferentes sistemas de escritura en Javascript](https://github.com/arualcat/ojos/diacritic)