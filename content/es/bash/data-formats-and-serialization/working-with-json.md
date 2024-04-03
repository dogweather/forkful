---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:33.560340-07:00
description: "Trabajar con JSON en programaci\xF3n Bash implica analizar, extraer\
  \ y manipular datos JSON directamente desde la l\xEDnea de comandos. Los programadores\
  \ a\u2026"
lastmod: '2024-03-13T22:44:59.268830-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con JSON en programaci\xF3n Bash implica analizar, extraer y manipular\
  \ datos JSON directamente desde la l\xEDnea de comandos."
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
Bash por sí solo carece de capacidades integradas de análisis de JSON, pero `jq` es un poderoso procesador JSON de línea de comandos que llena este vacío. Así es como se usa:

**Leyendo un archivo JSON:**

Ejemplo de `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Para leer y extraer el nombre del archivo JSON:
```bash
jq '.name' data.json
```
Salida:
```
"Jane Doe"
```

**Modificando datos JSON:**

Para actualizar la ciudad a "Los Angeles" y volver a escribir en el archivo:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Analizando JSON desde una variable:**

Si tienes JSON en una variable Bash, `jq` aún puede procesarlo:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Salida:
```
"John Doe"
```

**Trabajando con arreglos:**

Dado un arreglo de elementos en JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Para extraer el segundo elemento (la indexación comienza en 0):
```bash
jq '.items[1]' data.json
```
Salida:
```
"banana"
```

Para operaciones más complejas y filtros, `jq` tiene un manual comprensivo y tutoriales disponibles en línea, haciéndolo una herramienta versátil para todas tus necesidades de Bash/JSON.
