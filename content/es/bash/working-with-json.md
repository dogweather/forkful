---
title:                "Trabajando con JSON"
date:                  2024-01-19
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con JSON significa manipular un formato ligero de intercambio de datos. Los programadores lo usan porque es fácil de leer para humanos y simple de interpretar para máquinas.

## Cómo hacerlo:
Usaremos `jq`, una herramienta de línea de comandos para manipular JSON.

Instalación de `jq`:
```Bash
sudo apt-get update
sudo apt-get install jq
```

Suponiendo que tienes un archivo `datos.json`:
```JSON
{
  "nombre": "Juan",
  "edad": 30,
  "esProgramador": true
}
```

Para leer el campo `nombre`, ejecuta:
```Bash
cat datos.json | jq '.nombre'
```
Salida:
```
"Juan"
```

Para añadir un campo:
```Bash
jq '. + {"habilidades": ["bash", "python"]}' datos.json
```
Salida:
```JSON
{
  "nombre": "Juan",
  "edad": 30,
  "esProgramador": true,
  "habilidades": ["bash", "python"]
}
```

## Profundizando
JSON, acrónimo de JavaScript Object Notation, fue propuesto por Douglas Crockford en 2001. Aunque hay alternativas como XML, JSON es más ligero y su sintaxis es más sencilla. `jq` no es la única herramienta que existe para trabajar con JSON en Bash; `jshon` y `json.sh` son otras opciones, pero `jq` es notable por su potencia y flexibilidad.

## Ver También
- Manual oficial de `jq`: https://stedolan.github.io/jq/manual/
- JSON en Wikipedia: https://es.wikipedia.org/wiki/JSON
- Tutorial de Bash: https://linuxconfig.org/bash-scripting-tutorial
