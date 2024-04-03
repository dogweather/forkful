---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:49.084996-07:00
description: "C\xF3mo hacerlo: Bash no tiene una funci\xF3n integrada espec\xEDficamente\
  \ para capitalizar cadenas, pero puedes lograr esta tarea usando expansi\xF3n de\
  \ par\xE1metros o\u2026"
lastmod: '2024-03-13T22:44:59.227927-06:00'
model: gpt-4-0125-preview
summary: "Bash no tiene una funci\xF3n integrada espec\xEDficamente para capitalizar\
  \ cadenas, pero puedes lograr esta tarea usando expansi\xF3n de par\xE1metros o\
  \ herramientas externas como `awk`."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
Bash no tiene una función integrada específicamente para capitalizar cadenas, pero puedes lograr esta tarea usando expansión de parámetros o herramientas externas como `awk`. Aquí hay algunas formas de capitalizar una cadena en Bash:

**Usando expansión de parámetros:**

Este método manipula la cadena directamente en la shell.

```bash
str="hola mundo"
capitalizado="${str^}"
echo "$capitalizado"
```
Salida:
```
Hola mundo
```

**Usando `awk`:**

`awk` es una poderosa herramienta de procesamiento de texto disponible en la mayoría de los sistemas operativos tipo Unix, que se puede utilizar para capitalizar cadenas.

```bash
str="hola mundo"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Salida:
```
Hola mundo
```

**Usando `sed`:**

Para un enfoque más tradicional, se puede emplear `sed` para capitalizar la primera letra de una cadena. Sin embargo, es un poco más complejo en comparación con los métodos anteriores.

```bash
str="hola mundo"
echo "$str" | sed 's/./\u&/'
```
Salida:
```
Hola mundo
```

Estos fragmentos demuestran cómo capitalizar la primera letra de una cadena en Bash, destacando la flexibilidad de la programación en shell cuando se manipula texto.
