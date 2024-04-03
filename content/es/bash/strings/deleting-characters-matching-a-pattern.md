---
date: 2024-01-20 17:41:35.709161-07:00
description: "Eliminar caracteres siguiendo un patr\xF3n implica usar filtros para\
  \ quitar ciertos caracteres de strings de texto. Los programadores lo hacen para\
  \ limpiar\u2026"
lastmod: '2024-03-13T22:44:59.228928-06:00'
model: gpt-4-1106-preview
summary: "Eliminar caracteres siguiendo un patr\xF3n implica usar filtros para quitar\
  \ ciertos caracteres de strings de texto."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
Aquí algunos ejemplos de cómo eliminar caracteres con patrones usando Bash. Asume que estás utilizando la versión más reciente.

```Bash
# Eliminar todos los dígitos de una cadena
cadena="Usuario123"
echo "${cadena// [0-9]/}"

# Salida esperada: Usuario

# Eliminar solo los caracteres de 'a' a 'm'.
cadena="abcDEF123"
echo "${cadena// [a-m]/}"

# Salida esperada: DEF123

# Uso de `tr` para eliminar caracteres (comando externo a Bash)
cadena="Hello World 123"
echo "$cadena" | tr -d '0-9'

# Salida esperada: Hello World
```

## Análisis Profundo
Originalmente, Bash no era tan poderoso para las manipulaciones de texto avanzadas, pero con el tiempo y las actualizaciones, se han agregado características como parameter expansion y command substitution. Antes se dependía más de utilidades externas como `sed`, `awk` o `tr`. Aunque hoy en día Bash puede manejar muchas de estas tareas, aún se usan estas herramientas para acciones más complejas. La eliminación de caracteres con `tr` es más rápida y sencilla en algunos casos, pero hacerlo directamente en Bash con la sustitución de parámetros reduce la necesidad de llamadas a comandos externos, lo que puede mejorar la eficiencia en scripts.

## Ver También
- Bash Parameter Expansion: http://gnu.org/s/bash/manual/html_node/Shell-Parameter-Expansion.html
- `tr` command: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` intro: https://www.gnu.org/software/sed/manual/sed.html
- `awk` programming: https://www.gnu.org/software/gawk/manual/gawk.html
