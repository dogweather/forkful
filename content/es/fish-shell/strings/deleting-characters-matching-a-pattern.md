---
aliases:
- /es/fish-shell/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:42:02.027346-07:00
description: "Eliminar caracteres que coinciden con un patr\xF3n es filtrar texto\
  \ espec\xEDfico en base a reglas. Los programadores lo hacen para limpiar datos,\
  \ extraer\u2026"
lastmod: 2024-02-18 23:09:10.433983
model: gpt-4-1106-preview
summary: "Eliminar caracteres que coinciden con un patr\xF3n es filtrar texto espec\xED\
  fico en base a reglas. Los programadores lo hacen para limpiar datos, extraer\u2026"
title: "Eliminando caracteres que coinciden con un patr\xF3n"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Eliminar caracteres que coinciden con un patrón es filtrar texto específico en base a reglas. Los programadores lo hacen para limpiar datos, extraer información relevante o transformar formatos de texto.

## Cómo Hacerlo:

```Fish Shell
set frase "Hola, Mundo! 123"
echo $frase | string match -r "[A-Za-záéíóúÁÉÍÓÚñÑ ]+" # Devuelve solo letras y espacios
```

Salida:
```
Hola, Mundo 
```

Si quieres eliminar los números de la cadena:

```Fish Shell
echo $frase | string replace -r "[0-9]+" "" # Reemplaza dígitos por nada
```

Salida:
```
Hola, Mundo! 
```

## Inmersión Profunda:

Eliminar caracteres según un patrón no es algo exclusivo de Fish Shell; es un concepto usado en muchísimas herramientas de programación y procesamiento de texto, con raíces en las expresiones regulares que surgieron en los años 50. Alternativas comunes incluyen `grep`, `awk` y `sed` en Unix, pero Fish propone una sintaxis más simple y una integración nativa en su shell.

Fish usa la función `string` para manipular cadenas de texto, donde `string match` permite filtrar y `string replace` permite eliminar o reemplazar caracteres. La simplicidad de Fish le da la ventaja de ser más legible y fácil de usar en comparación con las herramientas más antiguas. Eso sí, la potencia y flexibilidad de, digamos, `sed`, sigue siendo impresionante y útil para los que saben cómo aprovecharla al máximo.

## Ver También:

- [Documentación oficial de Fish sobre la función `string`](https://fishshell.com/docs/current/cmds/string.html)
- [Tutorial de expresiones regulares](https://www.regular-expressions.info/tutorial.html)
- [Comparación entre Fish y otros shells](https://fishshell.com/docs/current/tutorial.html#tut_comparisons)
