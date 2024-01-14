---
title:    "TypeScript: Uniendo cadenas de texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos combinar diferentes cadenas de texto en una sola, ya sea para mostrar información al usuario, para construir una URL o simplemente para crear un mensaje personalizado. La concatenación de cadenas es una función esencial que nos permite unir de manera efectiva diferentes valores de cadena en una única variable. 

## Cómo hacerlo

La concatenación de cadenas se puede realizar de varias maneras en TypeScript. Una forma es utilizando el operador '+' para unir dos cadenas juntas. Por ejemplo: 

```TypeScript
let nombre = "María";
let apellido = "González";
let nombreCompleto = nombre + " " + apellido;
console.log(nombreCompleto); //Salida: María González
```

También podemos utilizar el método `.concat()` que nos permite unir múltiples cadenas en una sola:

```TypeScript
let mensaje = "Hola" 
let saludo = mensaje.concat(" ", nombre, "!");
console.log(saludo); //Salida: Hola María!
```

Otra opción es utilizar plantillas de cadena (string templates) para concatenar fácilmente diferentes valores en una sola cadena:

```TypeScript
let edad = 25;
let texto = `Tengo ${edad} años`;
console.log(texto); //Salida: Tengo 25 años
```

## Profundizando

Es importante tener en cuenta que al concatenar cadenas en TypeScript, el resultado será siempre una nueva cadena, ya que las cadenas son inmutables (no se pueden modificar). Esto significa que no podemos cambiar la cadena original sino que creamos una nueva cada vez que realizamos una concatenación. También es importante considerar el tipo de datos de las cadenas que estamos uniendo, ya que si uno de ellos es un número, se convertirá automáticamente a una cadena.

## Ver también

- [Documentación oficial de TypeScript sobre concatenación de cadenas](https://www.typescriptlang.org/docs/handbook/strings.html#string-concatenation)
- [Tutorial de concatenación de cadenas en TypeScript](https://riptutorial.com/es/typescript/example/8060/concatenando-cadenas)
- [Ejemplos de plantillas de cadena en TypeScript](https://www.tutorialsteacher.com/typescript/typescript-string-template)