---
title:                "TypeScript: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena

Es común en la programación tener que manipular cadenas de texto, y en algunas situaciones puede ser necesario capitalizar una cadena, es decir, convertir su primera letra en mayúscula. Esto puede ser útil, por ejemplo, cuando se quiere mostrar una palabra o un nombre en un formato más legible o estandarizado.

## Cómo capitalizar una cadena en TypeScript

En TypeScript, hay varias formas de capitalizar una cadena. Una de las maneras más sencillas es utilizando el método `toUpperCase()` que está disponible en la clase `String`. Este método convierte toda la cadena a mayúsculas, por lo que necesitaremos también utilizar el método `toLowerCase()` para convertir el resto de la cadena a minúsculas. Veamos un ejemplo:

```TypeScript 
let nombre: string = "juan";
let nombreCapitalizado: string = nombre.charAt(0).toUpperCase() + nombre.slice(1).toLowerCase();
console.log(nombreCapitalizado); // Juan
```

También podemos utilizar la función `slice()` para seleccionar solo la primera letra de la cadena y luego unirla con el resto de la cadena utilizando los respectivos métodos `toUpperCase()` y `toLowerCase()`. Veamos otro ejemplo:

```TypeScript
let apellido: string = "GARCÍA";
let apellidoCapitalizado: string = apellido.charAt(0).toUpperCase() + apellido.slice(1).toLowerCase();
console.log(apellidoCapitalizado); // García
```

Otra forma de capitalizar una cadena en TypeScript es utilizando una función, que recibirá la cadena como parámetro y devolverá la cadena capitalizada. Un ejemplo podría ser:

```TypeScript
function capitalizar(cadena: string): string {
    let primeraLetra: string = cadena.charAt(0).toUpperCase();
    let restoCadena: string = cadena.slice(1).toLowerCase();
    return primeraLetra + restoCadena;
}
let ciudad: string = capitalizar("mADrid");
console.log(ciudad); // Madrid
```

## Profundizando en la capitalización de cadenas

Si bien hemos visto algunas formas de capitalizar una cadena en TypeScript, es importante recordar que las cadenas son inmutables en este lenguaje, por lo que al utilizar los métodos `toUpperCase()` y `toLowerCase()` estamos creando una nueva cadena y no modificando la original. Además, es importante tener en cuenta que estas soluciones funcionan bien con cadenas simples, pero pueden no ser tan efectivas si tenemos en cuenta acentos o caracteres especiales en otros idiomas.

Es por esto que, si queremos una solución más robusta y que tenga en cuenta todas las posibles variaciones de una cadena, puede ser necesario utilizar librerías externas o crear una función más compleja que tenga en cuenta estas particularidades.

## Ver también

- [Documentación oficial de TypeScript sobre clase String](https://www.typescriptlang.org/docs/handbook/2/strings.html#string-methods)
- [Ejemplos de capitalización de cadenas en TypeScript](https://www.geeksforgeeks.org/string-methods-in-typescript/)
- [Implementación más compleja de capitalización de cadenas en TypeScript](https://medium.com/@JosephJnk/typescript-capitalizing-uppercase-and-lowercase-words-in-a-string-1333ce6e32a7)