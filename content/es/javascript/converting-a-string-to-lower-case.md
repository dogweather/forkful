---
title:    "Javascript: Convirtiendo una cadena a minúsculas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# ¿Por Qué Convertir una Cadena a Minúsculas en Javascript?

Si estás programando en Javascript, puede ser útil convertir una cadena de texto a minúsculas en ciertos casos. Por ejemplo, para comparar dos cadenas sin importar si están escritas en mayúsculas o minúsculas. En este artículo te explicaremos cómo hacerlo y te adentraremos en los detalles de por qué es útil.

## Cómo Convertir una Cadena a Minúsculas en Javascript
Para convertir una cadena a minúsculas en Javascript, podemos utilizar el método `toLowerCase()` que está disponible para cualquier cadena de texto.

```Javascript
let cadena = "Hola Mundo";
cadena = cadena.toLowerCase();
```

Esto modificará la variable `cadena` y la convertirá a `"hola mundo"` en minúsculas. También podemos usar este método al obtener una entrada del usuario para asegurarnos de que no importa cómo escriba la cadena, siempre la recibamos en minúsculas.

```Javascript
let entrada = prompt("Escribe tu nombre:");
entrada = entrada.toLowerCase();
console.log("¡Hola " + entrada + "!");
```

En este ejemplo, si el usuario escribe su nombre en mayúsculas, gracias a `toLowerCase()` el nombre será impreso en minúsculas en la consola.

## Profundizando en la Conversión de una Cadena a Minúsculas
Este método de `toLowerCase()` utiliza las reglas del lenguaje para convertir los caracteres a minúsculas. Esto significa que, por ejemplo, la letra "Á" en español se convertirá a "á" en minúsculas, y la letra "Ü" en alemán se convertirá a "ü". Además, este método no afecta a los números ni a los símbolos.

También es importante tener en cuenta que este método no modifica la cadena original, sino que crea una nueva cadena con los cambios. Por eso es que en los ejemplos anteriores asignamos el resultado a la misma variable.

## Ver También
- [Método `toLowerCase()` en la documentación de MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toLowerCase)
- [Ejemplo de uso de `toLowerCase()` en W3Schools](https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_tolowercase)