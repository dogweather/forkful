---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Javascript: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué: Eliminando caracteres que coinciden con un patrón?

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación, especialmente en JavaScript. Puede ser útil para limpiar y formatear datos, realizar validaciones y mucho más. Además, es una habilidad importante para tener en tu caja de herramientas de programación.

## Cómo hacerlo:

Eliminar caracteres que coinciden con un patrón en JavaScript puede hacerse de varias maneras, dependiendo de tus necesidades y preferencias. Aquí te mostramos dos opciones usando el método `replace()` y el operador `slice()`.

### Método replace()

El método `replace()` es una función de cadena que puede ser utilizada para reemplazar una parte de una cadena con una nueva cadena. Puede tomar dos argumentos: el primer argumento es el patrón que deseas buscar y el segundo argumento es la cadena de reemplazo. Aquí hay un ejemplo de cómo usarlo para eliminar todas las vocales de una cadena:

```Javascript
let frase = "Hola amigos";
let nuevaFrase = frase.replace(/[aeiou]/gi, "");
console.log(nuevaFrase); // Hl mgs
```

En el ejemplo anterior, usamos una expresión regular como patrón para buscar todas las vocales en la cadena. El modificador `g` significa que buscamos todas las coincidencias en lugar de solo la primera, y el modificador `i` significa que la búsqueda es insensible a mayúsculas y minúsculas. Luego simplemente reemplazamos todas las coincidencias con una cadena vacía, es decir, las eliminamos.

### Operador slice()

Otra forma de eliminar caracteres que coinciden con un patrón es usando el operador `slice()` en combinación con `indexOf()`. El operador `slice()` crea una nueva cadena extrayendo una parte de otra cadena, mientras que el método `indexOf()` devuelve la posición de la primera coincidencia de un patrón en una cadena. Aquí hay un ejemplo de cómo usarlos para eliminar todas las letras "a" de una cadena:

```Javascript
let mensaje = "Javascript es divertido";
let nuevaLetra = mensaje.indexOf("a");
while (nuevaLetra != -1) {
  mensaje = mensaje.slice(0, nuevaLetra) + mensaje.slice(nuevaLetra + 1);
  nuevaLetra = mensaje.indexOf("a");
}
console.log(mensaje); // Jvscrpt es dvertido
```

En este ejemplo, usamos un bucle while para ir eliminando todas las "a" encontradas en la cadena. En cada iteración, utilizamos `slice()` para cortar la parte de la cadena que queremos mantener (desde el inicio hasta la posición de la "a" encontrada) y la parte que queremos eliminar (desde la "a" hasta el final). Finalmente, asignamos la nueva cadena a la variable inicial para reemplazarla con la versión sin las "a".

## Deep Dive:

Existen otras formas de eliminar caracteres que coinciden con un patrón en JavaScript, como utilizar expresiones regulares en conjunción con otros métodos de cadena como `split()` y `join()`. Además, es importante recordar que algunos métodos de eliminación pueden modificar la cadena original, mientras que otros devuelven una nueva cadena sin modificar la original.

## Ver también:

- Documentación de Mozilla sobre el método `replace()`: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/replace
- Tutorial de W3Schools sobre el operador `slice()`: https://www.w3schools.com/jsref/jsref_slice_string.asp
- Ejemplos de uso de expresiones regulares en JavaScript: https://regexr.com/