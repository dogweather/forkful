---
title:                "Uniendo cadenas"
html_title:           "TypeScript: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Concatenar cadenas de texto es simplemente unir varias cadenas en una sola. Los programadores lo hacemos por varias razones, incluyendo la necesidad de construir mensajes, guardar datos y hacer operaciones matemáticas con números expresados como cadenas de texto.

## Cómo hacerlo:

```TypeScript
// Ejemplo 1: Concatenar dos cadenas de texto
let saludo: string = "¡Hola";
let nombre: string = "Alice!";
let mensaje: string = saludo + nombre;
console.log(mensaje); // Salida: ¡Hola Alice!

// Ejemplo 2: Concatenar cadenas y números
let edad: number = 29;
let leyenda: string = "Tengo";
let informacion: string = leyenda + edad + "años";
console.log(informacion); // Salida: Tengo 29 años
```

## Profundizando

La concatenación de cadenas de texto tiene su origen en los lenguajes de programación de bajo nivel, donde se utilizaba para formar mensajes y mostrar información al usuario. A medida que los lenguajes de programación evolucionaron, surgieron alternativas como plantillas de cadenas y expresiones regulares para realizar operaciones similares.

Sin embargo, la concatenación de cadenas sigue siendo una herramienta útil y común en la programación de hoy en día. En TypeScript, la concatenación se puede realizar usando el operador "+" o utilizando el método "concat()" de la clase String.

## Ver también

- [Documentación oficial de TypeScript](https://www.typescriptlang.org/docs/) 
- [Expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)