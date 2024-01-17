---
title:                "Buscando y reemplazando texto"
html_title:           "Javascript: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Buscar y reemplazar texto es una técnica común en programación que permite encontrar y reemplazar palabras o frases específicas en un archivo o en una cadena de texto. Los programadores utilizan esta técnica para editar y corregir rápidamente grandes cantidades de texto en su código.

## ¿Cómo hacerlo?

Para buscar y reemplazar en Javascript, se pueden utilizar dos métodos diferentes: el método `replace()` y el método `replaceAll()`. Ambos métodos toman dos argumentos: el texto a buscar y el texto a reemplazarlo.

```
// Ejemplo con el método replace()
let texto = "Me encanta programar en Javascript!";
let nuevoTexto = texto.replace("encanta", "gusta");
console.log(nuevoTexto); // Output: Me gusta programar en Javascript!

// Ejemplo con el método replaceAll()
let texto = "Javascript es un lenguaje de programación increíble!";
let nuevoTexto = texto.replaceAll("increíble", "fantástico");
console.log(nuevoTexto); // Output: Javascript es un lenguaje de programación fantástico!
```

## Profundizando

Buscar y reemplazar texto es una funcionalidad que se ha utilizado desde los primeros lenguajes de programación. En los primeros años, esta tarea se realizaba manualmente, agregando comandos para reemplazar palabras o frases específicas. Sin embargo, con el avance de tecnologías y de los lenguajes de programación, surgieron métodos como `replace()` y `replaceAll()` que simplificaron esta tarea.

Además de los métodos `replace()` y `replaceAll()` de Javascript, existen otras formas de buscar y reemplazar texto en diferentes lenguajes de programación. Por ejemplo, en Python se utiliza el método `replace()` y `re.sub()`, mientras que en PHP se utiliza la función `str_replace()`.

En cuanto a la implementación, el método `replace()` reemplaza solo la primera ocurrencia del texto buscado, mientras que `replaceAll()` reemplaza todas las ocurrencias.

## Ver también

- [Método replace() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Método replaceAll() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)