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

## ¡Qué y Por qué?
Trabajar con JSON en programación es una forma de intercambiar y almacenar datos de manera estructurada y legible para los humanos. Los programadores usan JSON debido a su simplicidad y adaptabilidad para una amplia gama de aplicaciones y sistemas.

## Cómo:
¡Comencemos a trabajar con JSON en Bash! Primero, necesitamos instalar la herramienta ```jq``` para manejar datos JSON. 
```Bash
sudo apt-get install jq
```
Una vez instalado, podemos utilizar comandos como ```jq '.key' archivo.json``` para obtener valores específicos de nuestro archivo JSON.

## Inmersión Profunda:
JSON, o JavaScript Object Notation, fue creado en la década de 1990 como un formato de intercambio de datos basado en JavaScript. Aunque todavía se usa ampliamente en aplicaciones web, ha ganado popularidad en otros ámbitos de la programación debido a su simplicidad y compatibilidad con una variedad de lenguajes de programación. Alternativas a JSON incluyen XML y YAML. En Bash, puedes usar comandos como ```sed``` y ```awk``` para manejar datos en formato JSON.

## Ver También:
Si quieres aprender más sobre JSON en Bash, aquí tienes algunos recursos útiles:
- Documentación de ```jq``` (https://stedolan.github.io/jq/manual)
- Un tutorial de Bash para principiantes con JSON (https://programminghistorian.org/es/lecciones/json-bash)
- Un artículo sobre el manejo de datos JSON en Bash (https://davidwalsh.name/json-bash)