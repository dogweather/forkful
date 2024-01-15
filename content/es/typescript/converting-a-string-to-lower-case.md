---
title:                "Convertir una cadena a minúsculas"
html_title:           "TypeScript: Convertir una cadena a minúsculas"
simple_title:         "Convertir una cadena a minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común que tengamos que manipular cadenas de texto y una de las tareas más comunes es convertir una cadena a minúsculas. Esto puede ser útil por varias razones, como por ejemplo para unificar el formato de los datos o para comparar cadenas de manera más eficiente.

## Cómo hacerlo

Para convertir una cadena a minúsculas en TypeScript, podemos usar el método `toLowerCase()`. Este método toma la cadena original y devuelve una nueva cadena con todas las letras en minúsculas. Veamos un ejemplo:

```TypeScript
let texto = "Hola Mundo";
let textoEnMinusculas = texto.toLowerCase();

console.log(textoEnMinusculas); // resultado: hola mundo
```

En este ejemplo, primero creamos una variable con una cadena de texto y luego usamos el método `toLowerCase()` para convertir esa cadena a minúsculas. Podemos ver el resultado en la consola, donde la cadena se imprime en minúsculas.

También podemos aplicar este método directamente a una cadena sin necesidad de una variable separada. Ejemplo:

```TypeScript
console.log("¡HOLA MUNDO!".toLowerCase()); // resultado: ¡hola Mundo!
```

En este caso, el método `toLowerCase()` se aplica directamente a la cadena "¡HOLA MUNDO!" y se imprime el resultado en la consola.

## Profundizando

Detrás de escena, el método `toLowerCase()` utiliza el estándar Unicode para realizar la conversión de mayúsculas a minúsculas. Esto significa que no solo funcionará para el alfabeto latino, sino para cualquier otro alfabeto que utilice el estándar Unicode.

Además, este método no solo convierte letras en mayúsculas a minúsculas, sino que también tiene en cuenta caracteres especiales, como la letra Ñ en español, que se convierte a ñ en minúsculas. Esto hace que sea una herramienta muy útil y robusta para trabajar con cadenas en cualquier idioma.

## Ver también

- [Documentación de TypeScript sobre el método toLowerCase()](https://www.typescriptlang.org/docs/handbook/strings.html#string-operations)
- [Explicación del estándar Unicode](https://unicode.org/standard/standard.html)