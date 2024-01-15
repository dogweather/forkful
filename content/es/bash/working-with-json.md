---
title:                "Trabajando con json"
html_title:           "Bash: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un desarrollador o estás aprendiendo a programar, seguramente has escuchado el término JSON en algún momento. JSON es un formato de intercambio de datos que se ha vuelto muy popular debido a su facilidad de uso y su flexibilidad. Si estás trabajando con APIs, manipulando datos en tus aplicaciones o simplemente quieres estar al día con las últimas tendencias en desarrollo web, aprender a trabajar con JSON es fundamental.

## Cómo hacerlo
Para trabajar con JSON en Bash, necesitas usar el comando `jq`, una herramienta de procesamiento de datos que te permite manipular y transformar estructuras de JSON de forma sencilla. A continuación te mostramos algunos ejemplos útiles de cómo usar `jq` para trabajar con JSON:

- Obtener un valor específico de una propiedad en un archivo JSON:
```
$ jq '.propiedad' archivo.json
```

- Filtrar un array en un archivo JSON:
```
$ jq '.[] | select(.child.property == "valor")' archivo.json
```

- Crear un nuevo objeto JSON a partir de otro:
```
$ jq '[.propiedad1, .propiedad2, .propiedad3] | {"nueva_propiedad": .}' archivo.json
```

- Transformar un archivo JSON usando una condición:
```
$ jq '. | if .propiedad > 10 then .estado = "activo" else .estado = "inactivo" end' archivo.json
```

El comando `jq` también te permite leer datos de una API, lo cual es muy útil si estás trabajando con aplicaciones web. Simplemente usa el parámetro `-r` para obtener la respuesta en un formato legible.

## Exploración en profundidad
Si quieres profundizar en el tema de trabajar con JSON en Bash, hay algunas cosas que debes tener en cuenta:

- Los archivos JSON deben estar correctamente formateados para que `jq` los pueda procesar. Si tienes problemas, siempre puedes validar la estructura JSON con una herramienta en línea como JSONLint.
- Puedes usar filtros complejos para acceder a datos específicos en un archivo JSON y realizar distintas operaciones.
- `jq` también te permite trabajar con archivos no válidos JSON, lo cual es muy útil si tienes que manejar datos que contienen errores de formato.

En resumen, trabajar con JSON en Bash es una habilidad muy útil que te permitirá manejar datos de forma sencilla y eficiente en tus proyectos de programación.

## Ver también
- Documentación oficial de `jq`: https://stedolan.github.io/jq/
- Tutorial de `jq` en línea: https://stedolan.github.io/jq/tutorial/