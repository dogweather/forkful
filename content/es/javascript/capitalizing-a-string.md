---
title:                "Capitalizar una cadena"
html_title:           "Javascript: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de texto en JavaScript es una práctica común en el desarrollo web que nos permite darle un formato específico a nuestros datos para mejorar la presentación y legibilidad de la información.

## Cómo hacerlo

Para capitalizar una cadena en JavaScript, podemos utilizar el método `toUpperCase()` que convierte todo el texto en mayúsculas. También podemos utilizar el método `charAt()` para obtener la primera letra de la cadena y convertirla a mayúscula. A continuación, concatenamos esa letra con el resto de la cadena en minúsculas utilizando el método `slice()`.

Veamos un ejemplo de cómo capitalizar una cadena en JavaScript:

```Javascript
let cadena = "hola mundo";
let primeraLetra = cadena.charAt(0).toUpperCase();
let restoDeCadena = cadena.slice(1).toLowerCase();
let cadenaCapitalizada = primeraLetra + restoDeCadena;

console.log(cadenaCapitalizada); // "Hola mundo"
```

Podemos encapsular esta lógica en una función para hacerla más reutilizable:

```Javascript
function capitalizar(cadena) {
  return cadena.charAt(0).toUpperCase() + cadena.slice(1).toLowerCase();
}

console.log(capitalizar("hola mundo")); // "Hola mundo"
console.log(capitalizar("estoy aprendiendo javascript")); // "Estoy aprendiendo javascript"
```

## Profundizando

Cuando utilizamos el método `toUpperCase()`, es importante tener en cuenta que este método no solo convierte las letras a mayúsculas, sino que también convierte los caracteres especiales y acentos a su equivalente en mayúsculas. Esto puede afectar el resultado final si estamos trabajando con diferentes idiomas y caracteres.

Además, si queremos capitalizar no solo la primera letra de la cadena, sino también la primera letra después de un espacio o un carácter de puntuación, necesitamos un enfoque diferente. Podemos utilizar el método `split()` para dividir la cadena en un array de palabras, luego utilizar el método `map()` y la función `capitalizar()` que definimos anteriormente para capitalizar cada palabra, y finalmente volver a unir el array en una cadena utilizando el método `join()`.

Veamos un ejemplo de cómo capitalizar todas las palabras de una cadena en JavaScript:

```Javascript
function capitalizarTodas(cadena) {
  let palabras = cadena.split(" ");
  let palabrasCapitalizadas = palabras.map(palabra => capitalizar(palabra));

  return palabrasCapitalizadas.join(" ");
}

console.log(capitalizarTodas("hola mundo")); // "Hola Mundo"
console.log(capitalizarTodas("estoy aprendiendo javascript")); // "Estoy Aprendiendo Javascript"
```

## Ver también

- [Documentación de MDN para el método `toUpperCase()` en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Documentación de MDN para el método `charAt()` en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Documentación de MDN para el método `slice()` en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Más información sobre el lenguaje JavaScript en nuestra sección dedicada](https://www.example.com/javascript)