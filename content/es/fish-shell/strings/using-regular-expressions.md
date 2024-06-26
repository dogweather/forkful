---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:37.073119-07:00
description: "C\xF3mo hacerlo: Aunque Fish Shell en s\xED no tiene un comando incorporado\
  \ para regex, utiliza efectivamente comandos externos como `grep`, `sed` y `awk`\
  \ que\u2026"
lastmod: '2024-03-13T22:44:59.490077-06:00'
model: gpt-4-0125-preview
summary: "Aunque Fish Shell en s\xED no tiene un comando incorporado para regex, utiliza\
  \ efectivamente comandos externos como `grep`, `sed` y `awk` que admiten regex,\
  \ lo que te permite incorporar operaciones regex en tus scripts."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
Aunque Fish Shell en sí no tiene un comando incorporado para regex, utiliza efectivamente comandos externos como `grep`, `sed` y `awk` que admiten regex, lo que te permite incorporar operaciones regex en tus scripts.

### Coincidencia de Patrones Básicos con `grep`
Buscar líneas en un archivo que coincidan con un patrón:

```fish
grep '^[0-9]+' myfile.txt
```

Este comando encuentra líneas que comienzan con uno o más dígitos en `myfile.txt`.

### Extrayendo y Reemplazando con `sed`
Extrae números de teléfono de un archivo:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Reemplaza todas las ocurrencias de "foo" con "bar" en `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Usando `string` para Regex Básicos
El comando `string` de Fish Shell admite operaciones simples de regex como coincidencia y reemplazo:

Coincidir un patrón en una cadena:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Salida:
```
3.1.2
```

Reemplazar dígitos siguientes a 'fish' con 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Salida:
```
Welcome to fish X.X.X
```

### Coincidencia Avanzada con `awk`
Imprimir la segunda columna de datos donde la primera columna coincide con un patrón específico:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Este comando busca líneas en `datafile` donde la primera columna comienza con una "a" seguida de uno o más dígitos e imprime la segunda columna.

Al integrar estos comandos externos, los programadores de Fish Shell pueden aprovechar toda la potencia de las expresiones regulares para tareas complejas de manipulación de texto, mejorando las capacidades nativas de la shell.
