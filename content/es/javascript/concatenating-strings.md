---
title:    "Javascript: Uniendo cadenas"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica muy útil en Javascript para combinar múltiples cadenas de texto en una sola. Esto puede ser útil en diversas situaciones, como en la creación de mensajes personalizados o en la generación de contenido dinámico para una página web.

## Cómo hacerlo

Para concatenar cadenas en Javascript, se utiliza el operador `+` y se colocan las cadenas que se desean unir entre comillas. Por ejemplo:

```Javascript
let saludo = "¡Hola ";
let nombre = "amigos!";
let mensaje = saludo + nombre;

console.log(mensaje); // Salida: ¡Hola amigos!
```

En este caso, se crearon dos variables `saludo` y `nombre` con cadenas de texto, y luego se concatenaron con el operador `+` en una tercera variable `mensaje`. Al imprimir `mensaje` en la consola, obtenemos la cadena completa.

Otra forma de concatenar cadenas es utilizando el método `.concat()` de Javascript. Este método toma como parámetros las cadenas que se desean unir y devuelve la cadena completa. Por ejemplo:

```Javascript
let frase1 = "La vida es ";
let frase2 = "una aventura.";
let frase3 = frase1.concat(frase2);

console.log(frase3); // Salida: La vida es una aventura.
```

También es posible concatenar más de dos cadenas a la vez, utilizando varias veces el operador `+` o el método `.concat()`.

## Profundizando

Es importante tener en cuenta que la concatenación de cadenas no solo se limita a texto literal, sino que también se pueden concatenar variables, expresiones o incluso otras funciones que devuelvan cadenas. Por ejemplo:

```Javascript
let numero1 = 5;
let numero2 = 7;
let suma = "La suma de " + numero1 + " y " + numero2 + " es " + (numero1+numero2);

console.log(suma); // Salida: La suma de 5 y 7 es 12.
```

En este caso, se combinaron cadenas con variables y expresiones matemáticas para generar un mensaje personalizado.

Es importante tener en cuenta que la concatenación de cadenas puede producir resultados inesperados si no se utilizan correctamente las comillas. Por ejemplo, si se olvidan las comillas alrededor de una cadena, esta será tratada como una variable y el resultado final no será el esperado.

## Ver también

- [Documentación oficial de concatenación de cadenas en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/concat)
- [Artículo sobre buenas prácticas de concatenación de cadenas en Javascript](https://www.sitepoint.com/javascript-concatenation-best-practices/)