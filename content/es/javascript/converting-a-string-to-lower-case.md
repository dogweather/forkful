---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertir cadenas en minúsculas con JavaScript

## ¿Qué y por qué?
La conversión de una cadena a minúsculas transforma todas las letras alfabéticas de una cadena a su equivalente en minúsculas. Los programadores lo hacen para normalizar datos y evitar problemas con la sensibilidad al caso.

## ¿Cómo hacerlo?
Con JavaScript, puedes hacerlo fácilmente con el método `toLowerCase()`. Aquí tienes un ejemplo:

```Javascript
let cadena = "Hola Mundo!";
let cadenaEnMinúsculas = cadena.toLowerCase();

console.log(cadenaEnMinúsculas); // "hola mundo!"
```
Como puedes ver, el texto "Hola Mundo!" se ha convertido a "hola mundo!".

## Profundizando
La función `toLowerCase()` de JavaScript se lanzó en 1997 junto con JavaScript 1.2. Desde entonces, ha sido un pilar en la manipulación de cadenas. Aunque siempre puedes escribir tu propia función para convertir una cadena a minúsculas, el uso de `toLowerCase()` es más eficiente y mantiene tu código limpio.

En JavaScript, no hay realmente una alternativa directa a `toLowerCase()`. Sin embargo, existen 'toUpperCase()' para convertir toda la cadena en mayúsculas y `localeCompare()` para la comparación insensible al caso.

Detrás de las escenas, `toLowerCase()` funciona a través de la tabla Unicode. El método simplemente mapea cada carácter alfabético en su equivalente de código bajo.

## Ver también
Para más detalles sobre `toLowerCase()`, revisa la [documentación oficial de Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toLowerCase). Si tienes problemas con caracteres acentuados, revisa este [artículo interesante sobre normalización de Unicode](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/).