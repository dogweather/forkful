---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?
Trabajar con JSON significa manipular datos en un formato ligero de intercambio, común en APIs y configuraciones. Los programadores lo usan por su simplicidad, facilidad de lectura y amplia adopción en distintas plataformas.

## Cómo hacerlo:

Manipular JSON en Fish Shell es sencillo gracias a herramientas como `jq`. Aquí unos ejemplos:

```Fish Shell
# Obtén un valor de un JSON simple
echo '{"nombre": "Juan", "edad": 30}' | jq '.nombre'
# Salida: "Juan"

# Asigna valor a variable en Fish
set user_edad (echo '{"nombre": "Juan", "edad": 30}' | jq '.edad')
echo $user_edad
# Salida: 30

# Modifica un JSON
echo '{"nombre": "Juan", "edad": 30}' | jq '.edad += 1'
# Salida: 
# {
#   "nombre": "Juan",
#   "edad": 31
# }
```

## Análisis en Profundidad

JSON (JavaScript Object Notation) se originó a partir de la necesidad de un formato de datos simple y legible para humanos, naciendo como una convención dentro de JavaScript pero adoptado universalmente más allá de este. Herramientas como `jq` han surgido para facilitar la manipulación de JSON en la línea de comandos, convirtiéndose en la opción estándar en la mayoría de entornos Unix-like. Al ser Fish Shell un intérprete de comandos moderno, aprovecha estas utilidades externas para operar con JSON, pero no incluye una herramienta integrada nativa para ello, a diferencia de otros datos como variables o listas.

## Ver También

- Documentación de `jq`: https://stedolan.github.io/jq/manual/
- Tutorial de Fish Shell sobre cómo trabajar con variables: https://fishshell.com/docs/current/index.html#variables-lists
- Guía oficial de JSON: https://www.json.org/json-es.html
