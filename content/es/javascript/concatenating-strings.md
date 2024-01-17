---
title:                "Concatenando cadenas"
html_title:           "Javascript: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¿Qué & Por qué?

Concatenar strings es combinar o unir múltiples strings en uno solo. Los programadores lo hacen para ahorrar tiempo y evitar tener que escribir cada string por separado. También es una forma de personalizar mensajes o páginas web.

# Cómo hacerlo:

```Javascript
var string1 = "¡Hola ";
var string2 = "mundo!";
var string3 = " Bienvenidos!";

// Usando el operador de concatenación "+" para unir los strings
console.log(string1 + string2 + string3);

// Output: ¡Hola mundo! Bienvenidos!

// También se puede utilizar el método ".concat()" para concatenar strings
console.log(string1.concat(string2).concat(string3));

// Output: ¡Hola mundo! Bienvenidos!
```

# Detalles más profundos:

- En los primeros años de la informática, concatenar strings era una tarea tediosa que requería mucha escritura. Con la introducción del operador "+" en el lenguaje de programación C en 1972, se volvió más fácil y rápido realizar esta tarea.

- Además de utilizar el operador "+" y el método ".concat()", también se puede utilizar el método ".join()", que permite especificar un separador entre los strings. Por ejemplo:

```Javascript
var array = ["Esto", "es", "una", "frase"]
console.log(array.join(" "))

// Output: Esto es una frase
```

- Al concatenar strings se pueden tener en cuenta los espacios en blanco entre ellos. Para evitar este problema, se pueden utilizar los métodos ".trim()", ".trimStart()" y ".trimEnd()" para eliminar los espacios.

# Ver también:

- [Métodos de string en Javascript](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [Operador "+" en Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition)
- [Método ".concat()" en Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Método ".join()" en Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)