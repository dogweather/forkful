---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:29.613852-07:00
description: "C\xF3mo hacerlo: **Leyendo un Archivo CSV L\xEDnea por L\xEDnea**."
lastmod: '2024-03-13T22:44:59.270057-06:00'
model: gpt-4-0125-preview
summary: "**Leyendo un Archivo CSV L\xEDnea por L\xEDnea**."
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:
**Leyendo un Archivo CSV Línea por Línea**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "Columna 1: $column1, Columna 2: $column2, Columna 3: $column3"
done < sample.csv
```

*Salida de muestra:*

```
Columna 1: id, Columna 2: nombre, Columna 3: correo electrónico
...
```

**Filtrando Filas CSV Basadas en una Condición**

Usando `awk`, puedes filtrar filas fácilmente. Por ejemplo, para encontrar filas donde la segunda columna sea igual a "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Modificando el Valor de una Columna**

Para cambiar la segunda columna a mayúsculas:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**Ordenando un Archivo CSV Basado en una Columna**

Puedes ordenar un archivo CSV basado en, digamos, la tercera columna (numéricamente):

```bash
sort -t, -k3,3n sample.csv
```

**Usando `csvkit` para Tareas Más Complejas**

`csvkit` es un conjunto de herramientas de línea de comandos para convertir y trabajar con CSV. Se puede instalar a través de pip.

Para convertir un archivo JSON a CSV:

```bash
in2csv data.json > data.csv
```

Para consultar un archivo CSV usando SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*Nota: Instalar `csvkit` requiere Python y se puede hacer usando `pip install csvkit`.*
