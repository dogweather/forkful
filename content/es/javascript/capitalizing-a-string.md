---
title:    "Javascript: Capitalizando una cadena"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

Al programar en Javascript, a menudo es necesario convertir una cadena de texto a mayúsculas. Aunque esto puede parecer una tarea simple, es importante conocer cómo hacerlo correctamente en el lenguaje de programación.

## Cómo hacerlo

Para capitalizar una cadena de texto en Javascript, se puede usar el método `toUpperCase()` que convierte todos los caracteres a mayúsculas. Por ejemplo:

```Javascript
let str = "hola mundo";
let capitalizado = str.toUpperCase();
console.log(capitalizado); // imprime "HOLA MUNDO"
```

También se puede usar el método `charAt()` para convertir solo la primera letra de la cadena a mayúscula, seguido de `slice()` para obtener el resto de la cadena en minúsculas. Esto es útil si se desea mantener un formato específico para ciertas palabras en la cadena. Por ejemplo:

```Javascript
let str = "hola mundo";
let capitalizado = str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
console.log(capitalizado); // imprime "Hola mundo"
```

## En profundidad

Es importante tener en cuenta que, al igual que en cualquier otro lenguaje de programación, Javascript es case-sensitive. Esto significa que las mayúsculas y minúsculas son tratadas como caracteres diferentes. Por lo tanto, al comparar cadenas de texto, es necesario asegurarse de que estén en el mismo formato.

Además, a partir de la versión ES6 de Javascript, existe un método `toLocaleUpperCase()` que ofrece soporte para idiomas específicos. Por ejemplo:

```Javascript
let str = "we love javascript";
let capitalizado = str.toLocaleUpperCase("en-US"); // imprime "WE LOVE JAVASCRIPT"
let capitalizadoFr = str.toLocaleUpperCase("fr-FR"); // imprime "WE LOVE JAVASCRIPT" (no hay cambios en francés)
```

## Ver también

- [Documentación de Microsoft para el método `toUpperCase()`](https://docs.microsoft.com/es-es/scripting/javascript/reference/touppercase-method-string-javascript)
- [Explicación detallada del método `toUpperCase()` en W3Schools](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [Ejemplos de uso del método `toLocaleUpperCase()` en la documentación de Mozilla](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)