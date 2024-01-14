---
title:                "TypeScript: Capitalizando una cadena"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo nos encontramos con la necesidad de modificar cadenas de texto para que se ajusten a un determinado formato, como por ejemplo, capitalizar la primera letra de cada palabra. Esto puede ser útil en situaciones como la creación de títulos o nombres de usuarios. Afortunadamente, en TypeScript, hay una función que nos permite hacer esto de manera rápida y sencilla.

## Cómo hacerlo

Para capitalizar una cadena de texto en TypeScript, utilizaremos el método `toUpperCase()`. Este método convierte todos los caracteres de una cadena a mayúsculas, pero no es exactamente lo que necesitamos. En su lugar, combinaremos este método con el método `slice()`, que nos permite seleccionar solo una parte de la cadena. Aquí hay un ejemplo de cómo hacerlo:
```
TypeScript
// Definimos una variable con la cadena de texto original
let original = "hola mundo";

// Convertimos la primera letra a mayúscula utilizando el método toUpperCase()
let primeraLetra = original[0].toUpperCase();

// Seleccionamos la parte restante de la cadena y la convertimos a minúsculas
let resto = original.slice(1).toLowerCase();

// Unimos la primera letra y el resto de la cadena para obtener el resultado final
let resultado = primeraLetra + resto;

// Imprimimos el resultado en la consola
console.log(resultado);

// La salida debería ser "Hola mundo"
```

Este es solo un ejemplo básico, pero puede ser aplicado a cadenas de texto más largas y complejas utilizando bucles y condicionales.

## Deep Dive

Si queremos profundizar aún más en el tema, es importante tener en cuenta que este método solo funciona correctamente con caracteres en inglés, ya que en otros idiomas, la capitalización puede variar dependiendo de la palabra y su contexto. Por lo tanto, si estamos trabajando con diferentes idiomas, es posible que necesitemos una solución más compleja que tenga en cuenta estas diferencias.

Además, en TypeScript también podemos utilizar la función `toLocaleUpperCase()` para respetar las reglas de capitalización de cada idioma. Esta función también acepta un parámetro para especificar el idioma deseado. Por ejemplo:
```
TypeScript
let original = "hello world";

// Utilizamos el parámetro "es" para especificar español como idioma
let convertido = original.toLocaleUpperCase("es");

console.log(convertido);

// La salida debería ser "HELLO WORLD" ya que en español, todas las letras son mayúsculas
```

## Ver también

- [Método toUpperCase() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Método slice() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Función toLocaleUpperCase() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)