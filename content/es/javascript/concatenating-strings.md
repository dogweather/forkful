---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La concatenación de cadenas en JavaScript simplemente involucra unir dos o más cadenas de texto en una sola. Los programadores recurren a esta técnica para mezclar variables con cadenas de texto al presentar mensajes al usuario, para garantizar que los datos sean legibles y bien formateados.

## Cómo :

La concatenación de cadenas puede lograrse principalmente de dos maneras:

- Utilizando el operador `+`.
```Javascript
let cadena1 = 'Hola,';
let cadena2 = '¡mundo!';
let saludo = cadena1 + " " + cadena2;
console.log(saludo); // Resultado: 'Hola, ¡mundo!'
```
- Utilizando plantillas literales (Backticks `).
```Javascript
let nombre = 'Juan';
let saludo = `Hola, ${nombre}!`;
console.log(saludo); // Resultado: 'Hola, Juan!'
```
El resultado es una cadena única producida uniendo las cadenas y/o variables en cuestión.

## Profundización :

La concatenación de cadenas ha sido una característica de JavaScript desde sus primeros días. Sin embargo, las plantillas literales se introdujeron recién en ES6, como una forma más limpia y eficiente de lograr la misma tarea, especialmente cuando se trabaja con varias variables.

Alternativas a la concatenación de cadenas incluyen métodos como `concat()`, aunque estos suelen ser menos populares debido a su sintaxis más larga.

```Javascript
let cadena1 = 'Hola,';
let cadena2 = '¡mundo!';
let saludo = cadena1.concat(" ", cadena2);
console.log(saludo); // Resultado: 'Hola, ¡mundo!'
```
Desde la perspectiva de la implementación, JavaScript internamente reconstruye las cadenas cada vez que se concatenan, lo que significa que la concatenación de cadenas puede tener un impacto en el rendimiento cuando se manejan grandes volúmenes de datos.

## Ver También : 

Para más detalles específicos sobre la concatenación de cadenas:

- [MDN Web Docs: String](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String)
- [MDN Web Docs: Template literals](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/template_strings)
- [JavaScript.info: Strings](https://es.javascript.info/string)