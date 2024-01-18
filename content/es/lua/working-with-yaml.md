---
title:                "Trabajando con yaml"
html_title:           "Lua: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

##
## ¡Descubre el Mundo de YAML en Lua!

### ¿Qué & Por qué?

Trabajar con YAML significa manipular un formato de datos legible por humanos utilizado para representar estructuras de datos en un texto plano. Los programadores usan YAML para almacenar y transmitir información de manera sencilla y conveniente.

### Cómo hacerlo:

```Lua
-- Importar la librería yaml
local yaml = require("yaml")

-- Crear una tabla con datos
local tabla = {
  nombre = "Juan",
  edad = 25,
  intereses = {"programación", "videojuegos", "viajar"}
}

-- Convertir la tabla en un string yaml
local string_yaml = yaml.dump(tabla)

-- Imprimir el string_yaml
print(string_yaml)

```

**Salida:**

```yaml
nombre: Juan
edad: 25
intereses:
- programación
- videojuegos
- viajar
```

### Profundizando:

**Contexto histórico:**

YAML fue creado en 2001 por Clark Evans, Ingy döt Net y Oren Ben-Kiki con el objetivo de ofrecer una alternativa legible y fácil de usar a otros formatos de datos. Se ha convertido en un lenguaje ampliamente utilizado en el mundo de la programación debido a su simplicidad y versatilidad.

**Alternativas:**

Existen muchos otros formatos de datos, como JSON y XML, que también son populares entre los programadores. Cada uno tiene sus propias ventajas y desventajas, por lo que depende del desarrollador elegir la herramienta más adecuada para su proyecto.

**Detalles de implementación:**

Para trabajar con YAML en Lua, se utiliza la librería [lua-yaml](https://github.com/gvvaughan/lua-yaml), que permite cargar y guardar archivos yaml, así como manipular los datos de forma sencilla.

### Ver también:

- [Sitio oficial de YAML](https://yaml.org/)
- [Documentación de lua-yaml](https://github.com/gvvaughan/lua-yaml)