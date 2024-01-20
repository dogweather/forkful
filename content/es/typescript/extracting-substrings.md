---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Extraer subcadenas implica seleccionar y copiar una parte específica de una cadena existente. Los programadores hacen esto para aislar y trabajar en segmentos de datos sin cambiar la cadena completa.

## ¿Cómo hacerlo?

Veamos cómo extraer una subcadena con TypeScript. Usaremos el método `substring()`:

```TypeScript
let cadena = "Hola, Mundo de TypeScript";
let subcadena = cadena.substring(7, 13);
console.log(subcadena);  
```

El código anterior genera la siguiente salida:

```shell
Mundo
```
Explicación: `substring(7, 13)` extrae la parte de la cadena desde el índice 7 hasta 12 (el índice 13 no se incluye).

## Análisis Profundo

**1) Contexto histórico**

El concepto de extraer subcadenas ha estado en la programación desde el principio. Sin embargo, los métodos para hacerlo han evolucionado con el tiempo y los lenguajes de programación modernos como TypeScript ofrecen maneras más eficientes.

**2) Alternativas**

En TypeScript puedes usar el método `substr()` además de `substring()`. La principal diferencia es que `substr()` utiliza la longitud del substring como segundo argumento en lugar de un índice final.

```TypeScript
let cadena = "Hola, Mundo de TypeScript ";
let subcadena = cadena.substr(7, 5);
console.log(subcadena); 
```

Salida:

```shell
Mundo
```

**3) Detalles de Implementación**

Tanto `substring()` como `substr()` no modifican la cadena original. Son no destructivas. No hay limitación en cuanto al tamaño de la subcadena que puedes extraer.

Por eficiencia, es mejor usar estos métodos en lugar de dividir y recomponer la cadena original.

## Ver también

Para más información sobre extracción de subcadenas, consulta:

1. [Documentación de TypeScript: metodo substring()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/substring)
2. [Documentación de TypeScript: método substr()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/substr)
3. [Substrings en TypeScript: Comprender con Ejemplos](https://www.geekhideout.com/dir/index.php)