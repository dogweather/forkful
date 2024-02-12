---
title:                "Buscando y reemplazando texto"
aliases:
- /es/typescript/searching-and-replacing-text.md
date:                  2024-01-20T17:58:47.472191-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Buscar y reemplazar texto es una operación común que modifica una cadena para sustituir partes específicas por otras. Los programadores lo usan para actualizar datos, corregir errores o cambiar formatos de manera eficiente, sin tener que revisar todo manualmente.

## Cómo:
```TypeScript
function reemplazarTexto(texto: string, buscar: string, reemplazo: string): string {
  return texto.replace(new RegExp(buscar, 'g'), reemplazo);
}

// Ejemplo de uso:
const textoOriginal = "Hola mundo, mundo cruel.";
const textoModificado = reemplazarTexto(textoOriginal, "mundo", "TypeScript");

console.log(textoModificado);
// Salida esperada: "Hola TypeScript, TypeScript cruel."
```

## Inmersión Profunda
En los 60, buscar y reemplazar era una tarea manual con papel y lápiz. Ahora, con lenguajes como TypeScript, usar expresiones regulares hace el trabajo rápido y potente. Alternativas a `replace` incluyen bibliotecas como Lodash, que ofrecen funciones con manejo de casos excepcionales más refinado. La implementación en TypeScript es generalmente directa, pero ten cuidado con las "peculiaridades" de las expresiones regulares, como escapar caracteres especiales y manejar casos globales con la bandera `'g'`.

## Véase También
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [MDN Web Docs on Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Lodash Library](https://lodash.com/)
