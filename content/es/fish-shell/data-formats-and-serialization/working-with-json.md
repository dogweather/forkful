---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:34.043756-07:00
description: "Trabajar con JSON en Fish Shell implica analizar y generar datos JSON,\
  \ una tarea com\xFAn para configurar aplicaciones, interacci\xF3n con API y optimizar\u2026"
lastmod: '2024-03-13T22:44:59.522686-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con JSON en Fish Shell implica analizar y generar datos JSON, una\
  \ tarea com\xFAn para configurar aplicaciones, interacci\xF3n con API y optimizar\
  \ flujos de trabajo en la l\xEDnea de comandos."
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
Fish Shell, por sí mismo, no tiene utilidades incorporadas para analizar y generar JSON. Sin embargo, se integra sin problemas con herramientas de terceros como `jq` para el procesamiento de JSON. `jq` es un procesador de JSON de línea de comandos potente y versátil que te permite cortar, filtrar, mapear y transformar datos estructurados con un lenguaje simple y expresivo.

### Analizando JSON con jq
Para analizar un archivo JSON y extraer datos usando `jq`:

```fish
# Asumiendo que tienes un archivo JSON llamado 'data.json' con el contenido: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Salida de muestra
"Fish Shell"
```

### Generando JSON con jq
Crear contenido JSON a partir de variables de shell o salidas:

```fish
# Crear objeto JSON a partir de variables
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Salida de muestra
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrando Colecciones JSON
Supongamos que tenemos un arreglo JSON de objetos en un archivo llamado `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Para filtrar este arreglo solo por versiones estables:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Salida de muestra
"3.1.2"
"3.4.0"
```

Los ejemplos proporcionados demuestran el poder de integrar `jq` con Fish Shell para operaciones JSON. Aprovechar tales herramientas enriquece la experiencia en la shell, haciéndola un entorno formidable para manejar formatos de datos modernos.
