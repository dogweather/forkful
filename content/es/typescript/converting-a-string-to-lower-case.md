---
title:    "TypeScript: Convirtiendo una cadena a minúsculas"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea común en la programación. Al hacerlo, podemos trabajar con la misma cadena de texto, independientemente de cómo los usuarios la ingresen. Esto también ayuda a estandarizar los datos y a simplificar su manipulación.

## Cómo hacerlo

El proceso básico para convertir una cadena de texto a minúsculas en TypeScript es utilizar el método `.toLowerCase()`. Veamos un ejemplo:

```TypeScript 
let texto = "Esto Es Una Cadena De Texto";
console.log(texto.toLowerCase());
```

El resultado en la consola será `esto es una cadena de texto`. 

También podemos utilizar este método en una variable que ya contenga una cadena de texto. Por ejemplo:

```TypeScript 
let nombre = "Juan";
console.log(nombre.toLowerCase());
```

El resultado en la consola será `juan`.

## Profundizando

El método `.toLowerCase()` en TypeScript utiliza la norma Unicode para convertir los caracteres a minúsculas. Esto significa que también funcionará con caracteres especiales de diferentes idiomas. 

También es importante tener en cuenta que este método no modifica directamente la cadena de texto original, sino que devuelve una nueva cadena de texto en minúsculas. Si queremos guardar el resultado en una variable, debemos asignarlo a una nueva variable o a la misma variable que contiene la cadena original. 

## Ver también

- [Documentación oficial de TypeScript sobre el método `.toLowerCase()`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-6.html#another-simple-convertre-ejemplo) 
- [Ejemplo de uso de `.toLowerCase()` en TypeScript](https://www.tutorialspoint.com/typescript/typescript_string_tolowercase.htm) 
- [Tutorial de TypeScript en español](https://www.freecodecamp.org/news/el-idioma-mas-facil-de-aprender-peso-programador-ada/)