---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar y Reemplazar Texto en JavaScript

## ¿Qué y Por Qué?

Buscar y reemplazar texto es una tarea común en la programación. Se utiliza para modificar cadenas de texto, lo que es útil para la transformación de datos y la manipulación de strings.

## Cómo hacerlo:

Ahora, daremos un vistazo a cómo se puede hacer esto en Javascript.

```Javascript
let cadena = "Hola, Mundo!";
let newCadena = cadena.replace("Mundo", "Javascript");
console.log(newCadena);
```
Este código reemplaza la palabra "mundo" por "Javascript". Así que el resultado sería:

```Javascript
"Hola, Javascript!"
```

Probemos con otra cadena de texto.

```Javascript 
let str = "La vida es corta para sentarse y mirar.";
let newStr = str.replace("sentarse y mirar", "no aprender de Javascript");
console.log(newStr);
```

Después de la ejecución, obtendremos el siguiente resultado:

```Javascript
"La vida es corta para no aprender Javascript."
```

## Inmersión Profunda

Historia: En sus inicios, JavaScript no tenía la función replace(). Los primeros desarrolladores debían recorrer la cadena y reemplazar manualmente cada ocurrencia.

Alternativas: Podemos usar la función `split` y `join` para reemplazar una cadena de texto. Sin embargo, esto puede ser más lento si trabajamos con cadenas largas.

Detalles de implementación: El método 'replace' no cambia la cadena original a la que se llama. En cambio, devuelve una nueva cadena. 'replace()' solo reemplaza la primera ocurrencia, para reemplazar todas las ocurrencias, necesitamos usar una expresión regular con la 'g'.

## Ver También

[Documentación Oficial del Método replace()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace)

[Explicación de la Expresión Regular de Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)

[Funciones split() y join() de JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/split)