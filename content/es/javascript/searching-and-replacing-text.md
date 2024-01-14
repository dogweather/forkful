---
title:    "Javascript: Búsqueda y reemplazo de texto"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

Si eres un programador en Javascript, probablemente hayas enfrentado el desafío de buscar y reemplazar texto en tu código. Ya sea para corregir errores o para realizar cambios en masa, la búsqueda y reemplazo de texto es una tarea común en la programación. Aprender cómo hacerlo correctamente te ayudará a ser más eficiente y a ahorrar tiempo en tu trabajo.

## Cómo hacerlo

Para buscar y reemplazar texto en Javascript, primero debes utilizar la función `replace()`. Esta función toma dos argumentos: el primer argumento es el texto que deseas buscar y el segundo argumento es el texto con el que deseas reemplazarlo. A continuación se muestra un ejemplo de cómo utilizar la función `replace()` para reemplazar todas las letras "a" con la letra "b" en un string:

```Javascript
let texto = "Hola amigo";
let nuevoTexto = texto.replace("a", "b");
console.log(nuevoTexto); // Salida: Hblb bmigo
```

Como se puede ver en el ejemplo, la función `replace()` reemplaza solo la primera coincidencia de la letra "a". Si deseas reemplazar todas las coincidencias, debes utilizar una expresión regular con el modificador "g" (global). A continuación se muestra un ejemplo de cómo hacerlo:

```Javascript
let texto = "Javascript es un lenguaje de programación";
let nuevoTexto = texto.replace(/a/g, "o");
console.log(nuevoTexto); // Salida: Jovoscript es un lenguoje de progromocon
```

Como se puede ver, ahora todas las letras "a" han sido reemplazadas por la letra "o". Puedes utilizar cualquier expresión regular dentro de la función `replace()` para realizar búsquedas más específicas y reemplazar texto de manera más compleja.

## Profundizando

La función `replace()` también puede ser utilizada con un callback para realizar reemplazos más avanzados. Este callback toma tres argumentos: el valor coincidente, el índice de la coincidencia en el string original y el string original completo. A continuación se muestra un ejemplo:

```Javascript
let numeros = "1 y 2";
let resultado = numeros.replace(/\d+/g, function(valor){
    return valor * 10;
});
console.log(resultado); // Salida: 10 y 20
```

En este ejemplo, se utiliza una expresión regular para encontrar los números en el string y luego se multiplica cada número por 10 en el callback. Puedes utilizar cualquier lógica en el callback para realizar reemplazos más complejos.

## Ver también

- [Documentación de la función `replace()` en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/replace)
- [Guía de expresiones regulares en w3schools](https://www.w3schools.com/js/js_regexp.asp)
- [Tutorial interactivo de expresiones regulares en RegexOne](https://regexone.com/)