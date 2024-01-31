---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

YAML (YAML Ain't Markup Language) es un formato de serialización de datos fácil de leer por humanos, usado comúnmente para configuración de aplicaciones. Lo usan los programadores por su claridad y compatibilidad con diferentes lenguajes de programación.

## Cómo hacerlo:

Para trabajar con YAML en Lua, necesitas una librería externa como 'lyaml'. Aquí un ejemplo básico:

```Lua
-- asegúrate de instalar lyaml con 'luarocks install lyaml'

local lyaml = require('lyaml')
local yaml_data = [[
nombre: Juan
edad: 30
pasatiempos:
  - programar
  - ciclismo
]]

-- Cargar datos YAML
local datos = lyaml.load(yaml_data)

-- Acceder a los valores
print(datos.nombre) -- salida: Juan
print(datos.edad) -- salida: 30

-- Muestra los pasatiempos
for index, hobby in ipairs(datos.pasatiempos) do
   print('Pasatiempo '..index..': '..hobby)
end
```

## Profundización:

YAML se originó en 2001. Su enfoque en la legibilidad lo hace popular, pero cuidado, es más lento que JSON o XML y puede ser inseguro si se permite la ejecución de código arbitrario durante la deserialización. Alternativas incluyen JSON, XML, TOML. Es importante escoger la librería correcta para prevenir problemas de seguridad y aprovechar características del lenguaje.

## Ver también:

- Documentación oficial de YAML: [https://yaml.org](https://yaml.org)
- lyaml en GitHub: [https://github.com/gvvaughan/lyaml](https://github.com/gvvaughan/lyaml)
- Alternativa de serialización JSON para Lua: [https://www.json.org/json-es.html](https://www.json.org/json-es.html)
