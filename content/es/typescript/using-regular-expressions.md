---
title:                "TypeScript: Utilizando expresiones regulares"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué usar expresiones regulares en TypeScript?

Las expresiones regulares son una herramienta poderosa para manipular y buscar patrones en cadenas de texto. En el contexto de TypeScript, esto puede ser especialmente útil al validar formularios, realizar búsquedas en bases de datos y realizar operaciones en grandes cantidades de datos. Su uso puede ahorrar tiempo y esfuerzo a la hora de manipular texto, y permite una mayor flexibilidad en la programación.

## Cómo utilizar expresiones regulares en TypeScript

Para empezar, es importante saber que en TypeScript se utilizan dos tipos de expresiones regulares: la sintaxis de JavaScript y la sintaxis de TypeScript. Ambas son muy similares, pero la sintaxis de TypeScript ofrece algunas mejoras y características adicionales.

Para crear una expresión regular en TypeScript, se utiliza el objeto `RegExp`. A continuación, se muestra un ejemplo de cómo se crearía una expresión regular para buscar una dirección de correo electrónico en una cadena de texto:

```TypeScript
let emailRegex: RegExp = new RegExp(/\w+@\w+\.\w+/);
let text: string = "¡Hola! Mi correo electrónico es ejemplo@correo.com.";
console.log(emailRegex.test(text));
```

En este código, se puede ver que la variable `emailRegex` contiene la expresión regular creada con la sintaxis de JavaScript, mientras que la variable `text` contiene una cadena de texto en la que se realizará la búsqueda. Al utilizar el método `test()` del objeto `RegExp`, se imprime en la consola el resultado de la búsqueda, que en este caso sería `true` ya que la cadena de texto contiene una dirección de correo electrónico.

Además de `test()`, también se pueden utilizar otros métodos, como `match()`, `replace()` y `split()`, para realizar diferentes tipos de manipulación de texto utilizando expresiones regulares.

## Profundizando en el uso de expresiones regulares

Una de las ventajas de utilizar expresiones regulares en TypeScript es que se pueden combinar múltiples patrones para realizar búsquedas más específicas. Por ejemplo, si se desea buscar un número de teléfono en una cadena de texto, se pueden utilizar los siguientes patrones:

- Números de teléfono con el formato "(555) 555-5555": `/\(\d{3}\) \d{3}-\d{4}/`
- Números de teléfono con el formato "555-555-5555": `/\d{3}-\d{3}-\d{4}/`
- Números de teléfono con el formato "1-555-555-5555": `/1-\d{3}-\d{3}-\d{4}/`

También se pueden utilizar expresiones regulares para validar formularios, como verificar que un campo de contraseña contenga al menos una letra mayúscula, una letra minúscula y un número:

```TypeScript
let passwordRegex: RegExp = new RegExp(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d).*$/);
```

Adicionalmente, es posible utilizar modificadores para realizar búsquedas con mayor precisión, como el modificador `i` para buscar sin tener en cuenta mayúsculas o minúsculas, o el modificador `g` para realizar varias búsquedas en una cadena de texto.

## Ver también

- [Documentación oficial de expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial de expresiones regulares en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-typescript)
- [Ejemplos prácticos de expresiones regulares en TypeScript](https://www.webslesson.info/2018/07/typescript-43-regular-expression-example.html)