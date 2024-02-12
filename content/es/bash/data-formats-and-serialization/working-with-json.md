---
title:                "Trabajando con JSON"
aliases: - /es/bash/working-with-json.md
date:                  2024-02-03T19:21:33.560340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con JSON en programación Bash implica analizar, extraer y manipular datos JSON directamente desde la línea de comandos. Los programadores a menudo hacen esto para integrar sin problemas scripts de shell con APIs web y formatos modernos de intercambio de datos, haciendo que los scripts Bash sean más poderosos y relevantes en un ecosistema pesado de JSON.

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
