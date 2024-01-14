---
title:    "Javascript: Eliminando caracteres que coincidan con un patrón"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

La eliminación de caracteres que coinciden con un patrón puede ser útil en situaciones donde se desean filtrar o limpiar datos no deseados. Por ejemplo, si estamos trabajando con un conjunto de datos y sólo queremos obtener información relevante, podemos utilizar esta técnica.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en JavaScript, podemos utilizar el método `replace()` combinado con expresiones regulares. Primero, creamos una expresión regular que coincida con los caracteres que deseamos eliminar. Luego, utilizamos el método `replace()` en una cadena de texto y le pasamos nuestra expresión regular y un carácter vacío para reemplazar los caracteres encontrados.

Veamos un ejemplo práctico:

```
// Creamos una expresión regular que coincida con los caracteres que deseamos eliminar
const patron = /[aeiou]/g;

// Utilizamos el método replace() en una cadena de texto y pasamos nuestra expresión regular y un carácter vacío
const texto = "Hola mundo!";
const textoSinVocales = texto.replace(patron, "");

console.log(texto); // "Hola mundo!"
console.log(textoSinVocales); // "Hl mnd!"
```

En este ejemplo, utilizamos la expresión regular `/[aeiou]/g` para coincidir con las vocales en la cadena de texto y luego las eliminamos utilizando el método `replace()`.

## Profundizando

Además de eliminar caracteres que coinciden con un patrón, también podemos utilizar expresiones regulares para realizar sustituciones más complejas en nuestras cadenas de texto. Por ejemplo, podemos utilizar grupos en nuestras expresiones regulares y luego utilizarlos en la sustitución.

Veamos otro ejemplo:

```
// Creamos una expresión regular para obtener los últimos dígitos de un número de tarjeta de crédito
const patron = /(\d{4})$/;

// Utilizamos el método replace() y reemplazamos los últimos dígitos con una serie de asteriscos (*)
const numeroTarjeta = "1234 5678 9012 3456";
const numeroTarjetaOculto = numeroTarjeta.replace(patron, "***$1");

console.log(numeroTarjeta); // "1234 5678 9012 3456"
console.log(numeroTarjetaOculto); // "1234 5678 9012 ***3456"
```

En este ejemplo, utilizamos la expresión regular `(\d{4})$` con un grupo de captura para obtener los últimos cuatro dígitos del número de tarjeta de crédito. Luego, en la sustitución, utilizamos el operador `$1` para insertar el grupo de captura y reemplazar los últimos dígitos con una serie de asteriscos.

## Ver también

- [Expresiones Regulares en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Método replace() en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace)