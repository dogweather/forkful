---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La conversión de un string a minúsculas es un proceso que cambia todos los caracteres alfabéticos en una cadena de texto a minúsculas. Los programadores lo hacen para normalizar datos, facilitar las comparaciones y prevenir errores.

## Como hacer:
Según TypeScript, se puede convertir un string a minúsculas usando el método `toLowerCase()`. Aquí tienes un ejemplo:

```TypeScript
let str = "Hola Mundo!";
let lowerStr = str.toLowerCase();
console.log(lowerStr);  // Output: "hola mundo!"
```
Asímismo, puedes convertir a minúsculas directamente en la línea de salida:
```TypeScript
console.log("Hola Mundo!".toLowerCase());  // Output: "hola mundo!"
```

## Inmersión Profunda
Desde los comienzos de la computación se han utilizado los métodos de conversión a minúsculas para asegurar la consistencia en los datos. A efectos de implementación, el método `toLowerCase()` simplemente analiza cada carácter en el string y, si es una letra mayúscula, lo reemplaza por su correspondiente minúscula.

En JavaScript, del cual TypeScript es un superconjunto, existen métodos alternativos como `toLocaleLowerCase()`, que respeta las reglas de localización de texto; en lenguajes donde ciertas letras pueden cambiar dependiendo de la localización, este método puede ser útil.

## Ver También
1. [Método toLowerCase() - Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. [Método toLocaleLowerCase() - Microsoft Docs](https://docs.microsoft.com/es-es/javascript/api/built-ins/string/tolocalelowercase?view=javascript-1.6)