---
title:                "Javascript: Convirtiendo una cadena a minúsculas"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador Javascript, probablemente ya sepas que las cadenas de texto son una parte importante de tu código. Pero, ¿alguna vez te has preguntado por qué a veces es necesario convertir una cadena a minúsculas? La respuesta es simple: a veces, necesitamos que nuestra lógica de programación no distinga entre mayúsculas y minúsculas en una cadena de texto.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Javascript, podemos utilizar el método `toLowerCase()` en la cadena. Este método devuelve una nueva cadena con todas las letras convertidas a minúsculas. Veamos un ejemplo:

```Javascript
let cadena = "¡Hola a todos!";
cadena = cadena.toLowerCase();
console.log(cadena); // output: ¡hola a todos!
```

En este ejemplo, la variable `cadena` se convierte a minúsculas y se guarda en la misma variable, pero también podríamos guardarla en una nueva variable. También es importante tener en cuenta que este método no afecta a la cadena original, sino que devuelve una nueva cadena con los cambios aplicados.

## Profundizando

El método `toLowerCase()` es sensible a idiomas, lo que significa que puede tener resultados diferentes dependiendo del idioma en el que trabajes. Por ejemplo, en inglés, la letra "I" mayúscula se convierte en "i" minúscula, mientras que en turco, la letra "I" mayúscula se convierte en "ı", una letra diferente. Esto se debe al concepto de "mayúsculas y minúsculas" es algo que varía entre idiomas.

También es importante tener en cuenta que el método `toLowerCase()` solo convierte letras en minúsculas, no otros caracteres como los símbolos o los números.

## Ver también

- [Método toLowerCase() - Documentación en línea de Javascript] (https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Diferencias de mayúsculas y minúsculas en diferentes idiomas] (https://en.wikipedia.org/wiki/Letter_case#Languages_with_both_a_binary_and_an_aplphabetically_ordered_case)

¡Esperamos que este artículo haya sido útil para entender por qué a veces necesitamos convertir cadenas a minúsculas en Javascript y cómo podemos hacerlo de manera efectiva!