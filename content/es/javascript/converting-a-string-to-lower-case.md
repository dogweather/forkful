---
title:                "Javascript: Convirtiendo un string a minúsculas"
simple_title:         "Convirtiendo un string a minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea común en la programación y puede ser útil para diversas aplicaciones, como búsqueda y comparación de cadenas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Javascript, se puede utilizar el método `toLowerCase()` de la clase String. Este método devuelve una nueva cadena en minúsculas, sin modificar la cadena original.

```Javascript
let cadena = "ESTE ES UN EJEMPLO";
let cadenaMinusculas = cadena.toLowerCase();

console.log(cadenaMinusculas);
// salida: este es un ejemplo
```

Este método también puede ser útil en la validación de entradas de usuario, ya que permite convertir cualquier entrada a minúsculas antes de compararla con una cadena predefinida, evitando así errores por mayúsculas y minúsculas.

```Javascript
let contraseña = "Secreto123";
let contraseñaUsuario = "secreto123";

if (contraseñaUsuario.toLowerCase() === contraseña.toLowerCase()) {
  console.log("Contraseña correcta!");
} else {
  console.log("Contraseña incorrecta!");
}
// salida: Contraseña correcta!
```

## Profundizando

Es importante tener en cuenta que el método `toLowerCase()` solo convierte caracteres en mayúsculas del alfabeto en caracteres en minúsculas. Los caracteres especiales, números o símbolos no se verán afectados.

Además, en algunas aplicaciones puede ser necesario utilizar el método `toLocaleLowerCase()` en lugar de `toLowerCase()`, ya que esto tendrá en cuenta las diferencias de mayúsculas y minúsculas según el idioma especificado.

Es importante también tener en cuenta que estos métodos no modifican la cadena original, sino que devuelven una nueva cadena en minúsculas, por lo que si se desea guardar el resultado en una variable, se deberá asignar el valor devuelto.

## Ver también

- [Documentación de MDN sobre toLowerCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Otros métodos útiles de la clase String en Javascript](https://www.w3schools.com/js/js_string_methods.asp)
- [Ejemplos prácticos de conversión de cadenas a minúsculas en Javascript](https://medium.com/@mayer13/5-ways-to-convert-string-to-lower-case-in-javascript-edfe9cd9b470)