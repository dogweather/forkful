---
title:                "Javascript: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena de texto en Javascript? 

La capitalización de cadenas de texto es una tarea común en la programación. Al capitalizar una cadena, se cambia la primera letra de cada palabra a mayúscula. Esto puede ser útil para mejorar la presentación de datos o para cumplir con ciertos criterios de formato. En este artículo, te explicaremos cómo capitalizar una cadena de texto en Javascript y profundizaremos en el proceso de capitalización.

## Cómo hacerlo: 

Para capitalizar una cadena de texto en Javascript, podemos utilizar el método `toUpperCase()` y `charAt()` de la clase String. Veamos un ejemplo de cómo capitalizar una cadena de texto utilizando estas dos funciones: 

```Javascript 
let cadena = "hola a todos";

// Convertir la primera letra de la cadena a mayúscula
let primeraLetra = cadena.charAt(0).toUpperCase();

// Obtener el resto de la cadena
let restoCadena = cadena.substring(1);

// Unir la primera letra capitalizada con el resto de la cadena
let cadenaCapitalizada = primeraLetra + restoCadena;

// Imprimir la cadena capitalizada
console.log(cadenaCapitalizada); // Imprimirá "Hola a todos"
```

En este ejemplo, utilizamos el método `toUpperCase()` para convertir la primera letra de la cadena a mayúscula y luego utilizamos `charAt()` para obtener el resto de la cadena. Finalmente, unimos la primera letra capitalizada con el resto de la cadena y tenemos nuestra cadena capitalizada.

## Profundizando en la capitalización de cadenas 

Es importante tener en cuenta que el método `toUpperCase()` sólo convierte la primera letra de una palabra en mayúscula. Si queremos capitalizar todas las palabras en una cadena, tendremos que utilizar un enfoque diferente. Por ejemplo, podemos dividir la cadena en un array de palabras utilizando el método `split()` y luego utilizar un bucle para capitalizar cada una de ellas.

Otra cosa a tener en cuenta es que el método `toUpperCase()` también convierte todos los caracteres de la cadena en mayúscula, por lo que si deseamos conservar minúsculas en algunas palabras, tendremos que utilizar otro método o enfoque para capitalizar sólo la primera letra.

En resumen, capitalizar una cadena de texto en Javascript es una tarea sencilla que puede lograrse utilizando los métodos `charAt()` y `toUpperCase()`. Sin embargo, es importante considerar los detalles mencionados anteriormente para obtener los resultados deseados.

## Ver también 

- [Método String toUpperCase() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Método String charAt() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Strings/charAt)
- [Método String split() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/split)