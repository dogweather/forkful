---
title:    "Javascript: Borrando caracteres que coinciden con un patrón"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez por qué alguien quisiera eliminar caracteres que coinciden con un patrón en Javascript? Bueno, aunque pueda parecer una tarea insignificante, hay algunas situaciones en las que es extremadamente útil. Por ejemplo, si tienes una cadena de texto larga y solo quieres extraer ciertas partes que siguen un patrón específico, o si necesitas limpiar una entrada de usuario antes de procesarla, eliminar caracteres que coinciden con un patrón puede ser una solución muy útil.

## Cómo hacerlo

Primero, vamos a definir el patrón que queremos eliminar. Digamos que queremos eliminar todos los números de una cadena de texto. Utilizaremos una expresión regular para definir este patrón, en este caso, /[0-9]/. Ahora, utilizando el método `replace()` en nuestra cadena, podemos reemplazar cada coincidencia de este patrón con una cadena vacía, lo que efectivamente elimina todos los números. Veamos un ejemplo:

```Javascript
let string = "Esta es una cadena con 123 números";
let newString = string.replace(/[0-9]/g, "");

console.log(newString); // Output: "Esta es una cadena con números"
```

Como puedes ver, todos los números han sido eliminados de la cadena original. Pero, ¿qué significa el parámetro `g` al final de nuestra expresión regular? Este parámetro indica que queremos reemplazar todas las coincidencias del patrón, no solo la primera. Si no lo especificamos, solo se eliminaría el primer número en nuestra cadena.

Pero ¿qué pasa si queremos eliminar no solo los números, sino también los espacios en blanco? Podemos utilizar múltiples patrones en nuestra expresión regular separados por el símbolo de pipe `|`. Veamos otro ejemplo:

```Javascript
let string = "Esta es una cadena con 123 números";
let newString = string.replace(/[0-9]| /g, "");

console.log(newString); // Output: "Estaesunacadenaconnúmeros"
```

Ahora, tanto los números como los espacios en blanco han sido eliminados de nuestra cadena.

## Profundizando

Como has visto en los ejemplos anteriores, utilizamos expresiones regulares para definir un patrón que queremos eliminar de nuestra cadena de texto. Pero, ¿qué son las expresiones regulares y cómo funcionan? En pocas palabras, son patrones escritos con una sintaxis especial que se utilizan para encontrar y manipular texto en una cadena. Puedes leer más sobre expresiones regulares en este [artículo de MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions).

También hemos utilizado el método `replace()` para reemplazar nuestras coincidencias con una cadena vacía, eliminándolas. Pero este método también nos permite reemplazar nuestras coincidencias con otra cadena. Puedes leer más sobre este método en este [artículo de MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/replace).

## Ver también

Esperamos que este artículo te haya ayudado a comprender cómo eliminar caracteres que coinciden con un patrón en Javascript y a entender mejor las expresiones regulares y el método `replace()`. Si quieres profundizar aún más, te recomendamos revisar estos enlaces:

- [Trabajo con cadenas de texto en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Guía de expresiones regulares en Javascript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)