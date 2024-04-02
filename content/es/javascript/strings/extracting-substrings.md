---
date: 2024-01-20 17:46:11.614329-07:00
description: "Extraer subcadenas es tomar trozos espec\xEDficos de una cadena de texto.\
  \ Los programadores lo hacen para analizar y manipular datos, como mostrar solo\
  \ un\u2026"
lastmod: '2024-03-13T22:44:59.447883-06:00'
model: gpt-4-1106-preview
summary: "Extraer subcadenas es tomar trozos espec\xEDficos de una cadena de texto.\
  \ Los programadores lo hacen para analizar y manipular datos, como mostrar solo\
  \ un\u2026"
title: "Extracci\xF3n de subcadenas"
weight: 6
---

## Qué y Por qué?

Extraer subcadenas es tomar trozos específicos de una cadena de texto. Los programadores lo hacen para analizar y manipular datos, como mostrar solo un nombre de usuario de una dirección de correo electrónico o obtener valores de una URL.

## Cómo hacerlo:

Javascript nos da algunas funciones para conseguirlo. Aquí van ejemplos con `slice()`, `substring()` y `substr()` (aunque `substr()` está en desuso, es útil conocerlo).

```Javascript
let texto = "HolaMundo";

// Usar slice(start, end)
console.log(texto.slice(1, 4)); // "ola"

// Usar substring(start, end)
console.log(texto.substring(4, 7)); // "Mun"

// Usar substr(start, length) - Obsoleto, pero aún se ve en código antiguo
console.log(texto.substr(4, 3)); // "Mun"
```

Nota que `slice()` puede tomar índices negativos, pero `substring()` no puede.

## Profundización

Antes de ES6, `substr()` era bastante común, aunque no parte del estándar ECMAScript. Fue reemplazado por `slice()` que ofrece más flexibilidad. Al manejar datos, especialmente donde el rendimiento es clave, saber qué función usar es esencial. Por ejemplo, `slice()` funciona bien con strings y arrays, mientras que `substring()` es exclusivo para strings. La implementación interna de estos métodos varía entre navegadores, pero todos buscan optimizar el rendimiento al manipular cadenas de texto.

## Ver También

Para más detalles sobre cómo trabajar con strings en Javascript, visita:

- MDN Web Docs sobre `slice()`: [MDN slice()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- MDN Web Docs sobre `substring()`: [MDN substring()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- MDN Web Docs sobre `substr()` (obsoleto): [MDN substr()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- Para comparar métodos de string, esta tabla es útil: [Comparative table](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String#comparing_methods)
