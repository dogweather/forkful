---
title:                "Búsqueda y reemplazo de texto"
html_title:           "TypeScript: Búsqueda y reemplazo de texto"
simple_title:         "Búsqueda y reemplazo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas razones por las que alguien podría querer buscar y reemplazar texto en su código. Algunas de las razones más comunes incluyen la corrección de errores de ortografía, la actualización de palabras clave o el cambio de nombres de variables para que sean más descriptivas.

## Cómo hacerlo

La sintaxis básica para buscar y reemplazar texto en TypeScript es la siguiente:

```TypeScript
const nuevoTexto = textoOriginal.replace("texto a buscar", "texto a reemplazar");
```

En este ejemplo, estamos usando el método `replace()` en una cadena de texto llamada `textoOriginal`. Este método toma dos argumentos: el texto que queremos buscar y el texto que queremos reemplazarlo. Si hay varias instancias del texto buscado, solo la primera será reemplazada.

También podemos usar expresiones regulares en lugar de una cadena de texto para realizar búsquedas y reemplazos más complejos. Por ejemplo:

```TypeScript
const nuevoTexto = textoOriginal.replace(/buscar/g, "reemplazar");
```

En este caso, estamos usando la expresión regular `/buscar/g`, que buscará todas las instancias de "buscar" en el texto original y las reemplazará con "reemplazar".

## Profundizando

Además de las opciones básicas mencionadas anteriormente, TypeScript también ofrece varias funciones adicionales para realizar búsquedas y reemplazos más avanzados.

Una de ellas es la opción de ignorar mayúsculas y minúsculas. Al agregar la letra `i` después de la expresión regular, se ignorarán las diferencias entre mayúsculas y minúsculas al realizar la búsqueda y el reemplazo.

Otra función útil es la posibilidad de utilizar grupos de captura en expresiones regulares. Esto significa que podemos seleccionar solo una parte del texto encontrado y usarlo en el reemplazo. Por ejemplo:

```TypeScript
const nuevoTexto = textoOriginal.replace(/(Mi)Textto A*$/g, "$1Texto B");
```

En este ejemplo, estamos utilizando una expresión regular para buscar todas las instancias de "MiTexto" seguidas de cualquier cantidad de "A". Luego, en el reemplazo, usamos el primer grupo de captura `(Mi)` para reemplazarlo con "MiTexto B", manteniendo la parte "Mi" del texto original.

## Ver también

Aquí hay algunos enlaces útiles para obtener más información sobre cómo buscar y reemplazar texto en TypeScript:

- Documentación oficial de TypeScript sobre el método `replace()`: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-0.html#replaceall-with-captures-in-replace-string-on-strings
- Ejemplos adicionales de uso de expresiones regulares en TypeScript: https://code.tutsplus.com/es/tutorials/8-regular-expressions-you-should-know-for-javascript--net-61456
- Una guía detallada sobre cómo usar expresiones regulares en TypeScript: https://www.digitalocean.com/community/tutorials/javascript-regular-expressions-for-web-designers