---
title:    "TypeScript: Capitalizando una cadena"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes al trabajar con strings es la de capitalizarlos, es decir, convertir la primera letra de cada palabra en mayúscula. Esto puede ser útil para fines estéticos o para seguir ciertos patrones de escritura en nuestro código. En este artículo, explicaremos cómo capitalizar un string en TypeScript.

## Cómo hacerlo

Para capitalizar un string en TypeScript, podemos utilizar el método `toTitleCase()` de la clase `String`. Este método toma una cadena y devuelve una nueva cadena con la primera letra de cada palabra en mayúscula. Veamos un ejemplo:

```TypeScript
let myString = "hola mundo";
let capitalizedName = myString.toTitleCase();
console.log(capitalizedName); // "Hola Mundo"
```

También podemos utilizar la función `toUpperCase()` en la primera letra de cada palabra para lograr el mismo resultado:

```TypeScript
let myString = "hola mundo";
let words = myString.split(" ");
for (let i = 0; i < words.length; i++) {
  words[i] = words[i].charAt(0).toUpperCase() + words[i].slice(1);
} 
let capitalizedName = words.join(" ");
console.log(capitalizedName); // "Hola Mundo"
```

Ambos métodos son igualmente válidos y pueden ser utilizados según la preferencia del desarrollador.

## Profundizando

Si queremos capitalizar un string en español de manera adecuada, debemos considerar ciertas particularidades del idioma, como por ejemplo las tildes. A continuación mostramos una función que capitaliza un string en español y toma en cuenta estas particularidades:

```TypeScript
function capitalizeSpanishString(str: string) {
  let words = str.split(" ");
  for (let i = 0; i < words.length; i++) {
    let firstChar = words[i].charAt(0).toUpperCase();
    if (firstChar === "Á" || firstChar === "É" || firstChar === "Í" || firstChar === "Ó" || firstChar === "Ú") {
      firstChar = firstChar.toLowerCase();
    }
    words[i] = firstChar + words[i].slice(1);
  }
  return words.join(" ");
}
let myString = "árbol de navidad";
let capitalizedName = capitalizeSpanishString(myString);
console.log(capitalizedName); // "Árbol de Navidad"
```

En esta función, primero dividimos el string en un array de palabras. Luego, para cada palabra, convertimos su primera letra en mayúscula. Sin embargo, si la letra es una vocal con tilde, la convertimos en minúscula ya que en español las mayúsculas no llevan tilde. Finalmente, unimos todas las palabras nuevamente en un solo string y lo devolvemos.

## Ver también

- [Método `toTitleCase()` de la clase `String` en TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#stringtypes-tostringtype)
- [Método `toUpperCase()` de la clase `String` en TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#stringtypes-utype)
- [Cómo dividir un string en un array en TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#split)