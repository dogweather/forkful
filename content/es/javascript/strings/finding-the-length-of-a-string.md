---
date: 2024-01-20 17:48:00.841434-07:00
description: "La longitud de una cadena en JavaScript se refiere a la cantidad de\
  \ caracteres que contiene. Los programadores frecuentemente necesitan esta informaci\xF3\
  n\u2026"
lastmod: '2024-03-13T22:44:59.449838-06:00'
model: gpt-4-1106-preview
summary: La longitud de una cadena en JavaScript se refiere a la cantidad de caracteres
  que contiene.
title: Calculando la longitud de una cadena
weight: 7
---

## How to:
En JavaScript, obtener la longitud de una cadena es pan comido. Usas la propiedad `.length` directamente en la cadena o en una variable que contenga la cadena. Aquí tienes unos ejemplos:

```javascript
let saludo = "Hola mundo";
console.log(saludo.length); // Salida: 10

console.log("Adiós".length); // Salida: 5

let vacio = "";
console.log(vacio.length); // Salida: 0
```

## Deep Dive
En los viejos tiempos de JavaScript, digamos en los años 90, `.length` ya estaba allí. Es uno de los aspectos más básicos y fundamentales del lenguaje. No necesitas ninguna biblioteca ni método especial, porque `.length` es parte del prototipo de `String`. Es rápido, eficiente y está disponible en cualquier cadena sin importar su procedencia.

Si quieres algo más que solo contar caracteres, como obtener la longitud de un arreglo, también usarías la propiedad `.length`. Fácil y consistente.

Alternativas a `.length` son pocas, ya que la mayoría intentarían reinventar la rueda. Podrías, en teoría, usar un bucle para contar cada carácter, pero eso es como tratar de cortar pan con una cuchara: poco práctico y nadie sabe por qué lo harías.

Respecto a la implementación, ten en cuenta que `.length` cuenta caracteres Unicode, lo que generalmente es lo que quieres. Pero puede haber sorpresas con caracteres especiales que están compuestos por dos unidades de código (llamados "surrogates"). Cada par cuenta como dos, lo que podría dar una longitud inesperada.

```javascript
// Ejemplo de emoji y surrogate pairs
console.log("👍".length); // Salida: 2

let complejo = "𠮷";
console.log(complejo.length); // Salida: 2
```

Estos casos son poco comunes, pero buenos de saber si trabajas con emojis o idiomas con caracteres complejos.

## See Also
- MDN Web Docs sobre la propiedad `length`: [MDN String.length](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Unicode y JavaScript: [JavaScript has a Unicode problem](https://mathiasbynens.be/notes/javascript-unicode)
- Información sobre `surrogate pairs`: [Understanding JavaScript’s UTF-16](https://flaviocopes.com/javascript-utf16/)
