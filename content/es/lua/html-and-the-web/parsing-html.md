---
title:                "Analizando HTML"
aliases:
- /es/lua/parsing-html.md
date:                  2024-02-03T19:12:29.563630-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Analizar HTML implica extraer datos e información de documentos HTML, lo cual es crucial para el raspado web, análisis de datos y tareas de automatización. Los programadores realizan esto para recopilar, analizar o manipular contenido web programáticamente, permitiendo la automatización de lo que de otro modo sería la extracción manual de datos de sitios web.

## Cómo hacerlo:
Lua no tiene una biblioteca integrada para analizar HTML, pero puedes utilizar bibliotecas de terceros como `LuaHTML` o aprovechar los enlaces para `libxml2` a través de `LuaXML`. Un enfoque popular es usar la biblioteca `lua-gumbo` para analizar HTML, que proporciona una capacidad de análisis compatible con HTML5, directa y sencilla.

### Instalando lua-gumbo:
Primero, asegúrate de que `lua-gumbo` esté instalado. Típicamente puedes instalarlo usando luarocks:

```sh
luarocks install lua-gumbo
```

### Análisis Básico con lua-gumbo:
Aquí te mostramos cómo puedes analizar un fragmento simple de HTML y extraer datos de él usando `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>¡Hola, mundo!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Salida: ¡Hola, mundo!
```

### Ejemplo Avanzado - Extrayendo Enlaces:
Para extraer atributos `href` de todos las etiquetas de anclaje (`<a>`) en un documento HTML:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Página de Ejemplo</title></head>
<body>
  <a href="http://example.com/1">Enlace 1</a>
  <a href="http://example.com/2">Enlace 2</a>
  <a href="http://example.com/3">Enlace 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Asegúrate de que sea un Elemento y tenga atributos
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Salida de Ejemplo:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Este fragmento de código itera a través de todos los enlaces en el documento e imprime sus atributos `href`. La capacidad de la biblioteca `lua-gumbo` para analizar y entender la estructura de un documento HTML simplifica el proceso de extracción de elementos específicos basados en sus etiquetas o atributos.
