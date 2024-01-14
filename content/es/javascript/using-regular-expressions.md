---
title:                "Javascript: Utilizando expresiones regulares"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar expresiones regulares?

Las expresiones regulares son una herramienta poderosa en la programación que te permiten buscar y manipular texto de manera efectiva. Son especialmente útiles cuando tienes que trabajar con grandes cantidades de texto o realizar acciones específicas en un conjunto de datos. Con el aprendizaje de expresiones regulares, podrás hacer tus programas más eficientes y precisos.

## Cómo utilizar expresiones regulares

Las expresiones regulares se escriben entre dos barras (//) y pueden incluir caracteres especiales para buscar patrones en el texto. Por ejemplo, si quieres encontrar todos los correos electrónicos en un texto, puedes utilizar la siguiente expresión regular:

```Javascript
/[\w\.]+@[\w\.]+/g
```

Esto buscará todas las combinaciones de letras, números y puntos seguidos de un símbolo "@" y otra combinación de letras, números y puntos. La "g" al final de la expresión indica que la búsqueda debe ser global, es decir, para todo el texto.

Otro uso común de las expresiones regulares es para validar datos de entrada en un formulario. Puedes utilizar una expresión regular para asegurarte de que un campo de correo electrónico, número de teléfono o código postal tenga el formato correcto antes de enviarlo.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares pueden parecer un poco intimidantes al principio, pero a medida que las vayas utilizando, te darás cuenta de su gran utilidad. Además de los caracteres especiales, también puedes utilizar cuantificadores para buscar patrones repetitivos, como "+" para indicar una o más repeticiones y "*" para indicar cero o más repeticiones.

También puedes utilizar grupos en tus expresiones regulares para buscar patrones específicos dentro de una cadena de texto. Por ejemplo, para buscar los números de teléfono en un texto en el formato (123) 456-7890, puedes utilizar la siguiente expresión regular:

```Javascript
/(\d{3}) \d{3}-\d{4}/g
```

El primer paréntesis indica que solo quieres buscar los tres primeros dígitos y el resto de la expresión busca el formato restante. Los resultados se devolverán en grupos para que puedas acceder a cada parte del número de teléfono.

## Ver también

Si quieres seguir aprendiendo sobre expresiones regulares, aquí te dejamos algunos recursos interesantes:

- [Documentación sobre expresiones regulares de Mozilla](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Tutorial interactivo de expresiones regulares en Codecademy](https://www.codecademy.com/courses/web-beginner-en-bH5s3/0/1)
- [Expresiones regulares en 30 minutos en RegexOne (en inglés)](https://regexone.com/)