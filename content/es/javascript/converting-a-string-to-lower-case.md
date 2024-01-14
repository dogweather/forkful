---
title:    "Javascript: Convirtiendo una cadena a minúsculas."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

##Por qué

Convertir una cadena de texto a minúsculas es una tarea común en programación. Con esta función, podemos asegurarnos de que todos los caracteres en una cadena sean uniformes y consistentes, lo que facilita la manipulación y comparación de palabras en nuestro código.

##Cómo hacerlo

Para convertir una cadena de texto a minúsculas en JavaScript, podemos utilizar el método `toLowerCase()`. Este método toma la cadena original y la convierte en una nueva cadena con todos los caracteres en minúsculas. Veamos un ejemplo de cómo se usaría este método en una función:

```Javascript
function convertirAMinusculas(cadena) {
  return cadena.toLowerCase();
}

console.log(convertirAMinusculas("Hola Mundo")); // Output: hola mundo
```

En este ejemplo, hemos utilizado la función `console.log()`para imprimir el resultado de nuestra función en la consola. Como puedes ver, la cadena "Hola Mundo" se ha convertido a minúsculas con éxito.

También podemos utilizar el método `toLowerCase()`en una cadena de texto existente, como por ejemplo en una variable:

```Javascript
var nombre = "MARÍA";

console.log(nombre.toLowerCase()); // Output: maría
```

Nuevamente, la cadena "MARÍA" se ha convertido a minúsculas utilizando el método `toLowerCase()`.

##Profundizando

Existen otras formas de convertir una cadena de texto a minúsculas en JavaScript, como por ejemplo utilizando el método `replace()`. Este método reemplaza una parte de una cadena con otra, lo que nos permite convertir todos los caracteres a minúsculas de manera más específica.

En lugar de utilizar el método `toLowerCase()` en toda la cadena, podemos utilizar `replace()` para reemplazar todas las letras mayúsculas con su versión en minúsculas en lugar de simplemente convertir toda la cadena:

```Javascript
var mensaje = "Hola A todos";
var mensajeConvertido = mensaje.replace(/[A-Z]/g, function(letra) {
  return letra.toLowerCase();
});

console.log(mensajeConvertido); // Output: hola a todos
```

En este ejemplo, hemos utilizado una expresión regular para seleccionar todas las letras mayúsculas en la cadena `mensaje` y reemplazarlas con su versión en minúsculas utilizando una función de reemplazo.

##Ver también

Si deseas profundizar aún más en el tema de las cadenas de texto en JavaScript, aquí hay algunos enlaces que podrían resultarte útiles:

- [Mozilla Developer Network: String.toLowerCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toLowerCase)
- [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [Aprende JavaScript con MentoriaJS: Cadenas](https://mentoriajs.com/cadenas-js/)