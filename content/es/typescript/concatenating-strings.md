---
title:                "TypeScript: Uniendo cadenas de caracteres"
simple_title:         "Uniendo cadenas de caracteres"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica esencial en la programación para combinar múltiples cadenas en una sola. Es especialmente útil al trabajar con datos de usuario o para mostrar información en un formato legible para el usuario.

## Cómo hacerlo

Para concatenar cadenas en TypeScript, podemos utilizar el operador ```+``` o el método ```concat()```. Veamos un ejemplo de cada uno:

```TypeScript
let nombre = "Marta";
let apellido = "García";
let nombreCompleto = nombre + " " + apellido;
console.log(nombreCompleto);
```
Esto producirá una salida de "Marta García". Aquí, usamos el operador ```+``` para unir las variables ```nombre``` y ```apellido``` junto con un espacio en blanco para obtener el nombre completo.

```TypeScript
let ciudad = "Madrid";
let pais = "España";
let direccion = ciudad.concat(", ", pais);
console.log(direccion);
```
Este código producirá una salida de "Madrid, España". Usamos el método ```concat()``` para unir las variables ```ciudad``` y ```pais```, separándolas con una coma y un espacio.

En ambos casos, podemos concatenar más de dos cadenas a la vez simplemente agregando más variables o cadenas al final del operador ```+``` o dentro del método ```concat()```. También podemos usar el método ```concat()``` con una cadena vacía ```""``` para unir múltiples cadenas y asegurarnos de que no hayan espacios extra en la salida.

## Profundizando

En TypeScript, también podemos usar plantillas de cadenas para concatenar variables directamente en una cadena, agregando el prefijo ```${}``` y colocando la variable o expresión entre los corchetes. Esto puede ser útil cuando tenemos una cadena larga que necesita incluir datos dinámicos. Veamos un ejemplo:

```TypeScript
let numMensajes = 10;
console.log(`Tienes ${numMensajes} nuevos mensajes`);
```
Esto producirá una salida de "Tienes 10 nuevos mensajes". Aquí, usamos la plantilla de cadena para agregar la variable ```numMensajes``` dentro de la cadena sin tener que usar el operador ```+``` o el método ```concat()```.

También es importante tener en cuenta que al concatenar números en una cadena, TypeScript los convertirá automáticamente a cadenas. Por ejemplo:

```TypeScript
let num1 = 5;
let num2 = 3;
console.log("El resultado es " + num1 + num2);
```
Esto producirá una salida de "El resultado es 53". En lugar de sumar los dos números, TypeScript los convierte en cadenas y los concatena juntos. Para obtener el resultado deseado, podemos utilizar plantillas de cadenas o convertir explícitamente los números en cadenas.

## Ver también

- [Documentación oficial de TypeScript sobre concatenar cadenas](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Tutorial de concatenar cadenas en TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Ejemplos de concatenar cadenas en TypeScript](https://stackabuse.com/concatenation-of-strings-in-typescript/)