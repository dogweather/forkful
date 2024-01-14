---
title:    "Javascript: Utilizando expresiones regulares"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en programación

Las expresiones regulares son una herramienta muy útil en la programación, ya que permiten buscar y manipular patrones de texto de manera eficiente y precisa. Con ellas, se pueden realizar tareas como validar datos de entrada, extraer información específica de un texto y realizar búsquedas y reemplazos en grandes conjuntos de datos. Por estas razones, el uso de expresiones regulares es muy común en todo tipo de proyectos de desarrollo.

## Cómo utilizar expresiones regulares en Javascript

Para utilizar expresiones regulares en Javascript, se pueden utilizar los métodos `match()`, `test()`, `replace()` y `split()` del objeto `RegExp`. A continuación se presentan algunos ejemplos de cómo utilizar estas expresiones:

```Javascript
// Validar si una cadena contiene un número de teléfono con el formato xxx-xxx-xxxx
var string = "555-555-5555";
var regex = /\d{3}-\d{3}-\d{4}/;
console.log(regex.test(string));  // output: true

// Extraer el código de área de una dirección de correo electrónico
string = "johndoe@example.com";
regex = /@(.*)\./;
console.log(regex.exec(string)[1]);  // output: example

// Reemplazar todos los espacios en blanco por guiones en una URL
string = "https://www.example.com/my page.html";
regex = /\s/g;
console.log(string.replace(regex, "-"));  // output: https://www.example.com/my-page.html

// Dividir una cadena en un arreglo utilizando un carácter delimitador
string = "1, 2, 3, 4, 5";
regex = /\s*,\s*/;
console.log(string.split(regex));  // output: ["1", "2", "3", "4", "5"]
```

## Un vistazo más profundo a las expresiones regulares

Las expresiones regulares incluyen un conjunto de caracteres especiales que representan patrones específicos, como dígitos `\d`, letras `\w` o espacios en blanco `\s`. Estos caracteres pueden combinarse con cuantificadores como `+` o `*` para especificar la cantidad de veces que un patrón debe aparecer en un texto. Además, se pueden aplicar modificadores como `i` para hacer que la búsqueda sea insensible a mayúsculas y minúsculas. La utilización de grupos de captura y retrocesos permite extraer información específica de un patrón identificado.

Es importante tener en cuenta que una expresión regular puede ser muy poderosa, pero también puede ser compleja y difícil de entender. Por esta razón, es recomendable utilizar recursos como expresionesregulares.info, regex101.com o el método `debug()` de la consola de desarrollo para ayudar en la construcción y depuración de expresiones regulares.

## Ver también

- [Documentación de expresiones regulares en Javascript en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Expresiones regulares en Javascript: una guía para principiantes](https://www.freecodecamp.org/news/regex-guide-es/)
- [Expresiones regulares en Javascript: cómo escribirlas y depurarlas](https://www.digitalocean.com/community/tutorials/how-to-write-effective-regular-expressions-in-javascript)