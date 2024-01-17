---
title:                "Capitalizando una cadena"
html_title:           "TypeScript: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En programación, capitalizar una cadena de texto significa convertir la primera letra de cada palabra en mayúscula. Los programadores suelen hacer esto para mejorar la legibilidad y la estética de su código.

## ¿Cómo hacerlo?
En TypeScript, puedes capitalizar una cadena utilizando el método integrado `toUpperCase()` junto con el método `charAt()` para obtener la primera letra de cada palabra. Aquí hay un ejemplo de código que capitaliza una cadena y muestra el resultado por consola:

```TypeScript
const cadena = "este es un ejemplo";
const palabras = cadena.split(" ");
let resultado = "";

for (let i = 0; i < palabras.length; i++) {
  resultado += palabras[i].charAt(0).toUpperCase() + palabras[i].slice(1) + " ";
}

console.log(resultado); // Output: Este Es Un Ejemplo
```

## Profundizando
La capitalización de cadenas ha sido utilizada durante mucho tiempo en lenguajes de programación para mejorar la legibilidad del código. Sin embargo, también existen otros métodos de formato de texto, como el camelCase o el snake_case, que tienen ventajas en ciertas situaciones.

En TypeScript, también puedes usar la función `replace()` junto con expresiones regulares para capitalizar ciertos patrones de texto en una cadena. Además, hay bibliotecas externas disponibles que ofrecen una amplia variedad de funciones de formato de texto más avanzadas.

## Ver también
Puedes encontrar más información sobre la capitalización de cadenas y otros métodos de formato de texto en la documentación oficial de TypeScript y en otras fuentes en línea, como blogs y foros de programadores.