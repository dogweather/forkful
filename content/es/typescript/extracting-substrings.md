---
title:                "Extrayendo subcadenas"
html_title:           "TypeScript: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Extraer subcadenas es una operación común en programación que implica obtener una parte de una cadena de texto más larga. Los programadores hacen esto para manipular y obtener información específica de una cadena, lo que ahorra tiempo y simplifica el proceso de manipulación de datos.

## Cómo hacerlo:
Los siguientes ejemplos muestran cómo extraer subcadenas en TypeScript utilizando el método ```.substring()```.

```TypeScript
let texto = "Este es un ejemplo de cadena";
console.log(texto.substring(0, 4)); // Salida: "Este"
console.log(texto.substring(8, 10)); // Salida: "un"
console.log(texto.substring(17)); // Salida: "ejemplo de cadena"
```
En el primer ejemplo, especificamos los índices para extraer desde el inicio (0) hasta el final del primer espacio (4), lo que nos devuelve la primera palabra "Este". En el segundo ejemplo, especificamos los índices para extraer desde el segundo espacio (8) hasta el tercer espacio (10), lo que nos devuelve la segunda palabra "un". Y en el tercer ejemplo, solo especificamos el índice inicial (17) y nos devuelve todo el texto a partir de ese índice hasta el final.

## Profundizando:
El método ```.substring()``` es un método heredado de JavaScript y también está disponible en otros lenguajes de programación. Sin embargo, en TypeScript, también podemos utilizar la sintaxis de "slice" para extraer subcadenas. Por ejemplo:
```TypeScript
let texto = "Este es otro ejemplo de cadena";
console.log(texto.slice(-6)); // Salida: "cadena"
```
Aquí, utilizamos números negativos para contar desde el final de la cadena. También es importante tener en cuenta que tanto ```.substring()``` como "slice" no modifican la cadena original, sino que devuelven una nueva cadena.

## Ver también:
- Documentación oficial de TypeScript sobre el método ```.substring()``` (https://www.typescriptlang.org/docs/handbook/working-with-strings.html#substrings)
- Comparación entre el método ```.substring()``` y "slice" en TypeScript (https://www.baeldung.com/substring-vs-slice-typescript)