---
title:    "TypeScript: Convirtiendo una fecha en una cadena."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto puede ser un paso común en la programación en TypeScript. Esto puede ser útil para mostrar la fecha en un formato específico o para almacenarla en una base de datos. Entender cómo realizar esta conversión puede ayudar a mejorar tus habilidades de programación y a trabajar de manera más eficiente.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en TypeScript, podemos utilizar el método `toString()`. Este método toma la fecha y la convierte en una cadena de texto con el formato `MMM DD AAAA, HH:MM:SS`. Veamos un ejemplo de cómo usar este método:

```TypeScript
let fecha = new Date();
let fechaCadena = fecha.toString();
console.log(fechaCadena); // Output: Tue Jun 22 2021, 11:00:00
```

También podemos especificar un formato personalizado para la cadena de texto utilizando el método `toLocaleString()`. Este método acepta dos parámetros: el idioma y opciones de formato. Veamos un ejemplo de cómo usarlo:

```TypeScript
let fecha = new Date();
let opciones = { year: 'numeric', month: 'long', day: 'numeric' };
let fechaCadena = fecha.toLocaleString('es-ES', opciones);
console.log(fechaCadena); // Output: 22 de junio de 2021
```

Podemos jugar con las opciones de formato para obtener diferentes resultados. Por ejemplo, si queremos mostrar solo el mes y el año, podemos usar las siguientes opciones: `{ year: 'numeric', month: 'long' }`, lo cual nos daría como resultado `junio de 2021`.

## Profundizando

Convertir una fecha en una cadena de texto puede parecer un proceso simple, pero hay algunas cosas que debemos tener en cuenta. Por ejemplo, el método `toString()` devuelve la fecha y hora en formato UTC, mientras que el método `toLocaleString()` devuelve la fecha y hora en el huso horario local. Esto puede causar confusiones si no se tiene en cuenta.

También es importante tener en cuenta que el formato de la cadena de texto puede variar según el idioma y la región configurada en el sistema. Por lo tanto, es importante usar el método `toLocaleString()` y especificar el idioma y las opciones de formato para obtener resultados consistentes.

## Ver también

- [Método toString() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date-prototype-tostring)
- [Método toLocaleString() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date-prototype-tolocalestring)