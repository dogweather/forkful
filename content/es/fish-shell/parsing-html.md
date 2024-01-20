---
title:                "Análisis de HTML"
date:                  2024-01-20T15:31:35.145831-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Parsear HTML es leer y extraer datos de documentos HTML. Los programadores lo hacen para manipular o acceder a contenido web de forma programática.

## Cómo hacerlo:
No existe un comando "nativo" en Fish para parsear HTML, pero puedes usar herramientas de línea de comandos como `pup`, `hxselect`, o `tidy` junto con Fish para trabajar con HTML. Aquí va un ejemplo con `pup`:

```Fish
curl -s "https://ejemplo.com" | pup 'div.clase-ejemplo text{}'
```

Imaginemos que esto devuelve:

```
Hola, soy un texto de ejemplo dentro de un div con clase.
```

## Profundidad:
Parsear HTML en la shell a menudo implica herramientas externas porque el HTML puede ser complejo y no está diseñado para ser procesado por shells. En el pasado, herramientas como `grep` o `awk` se abusaban para este fin, pero no estaban diseñadas para HTML estructurado. `pup` es específico de la shell, mientras que `hxselect` forma parte de HTML-XML-utils y `tidy` ayuda a limpiar y estructurar bien el HTML. Todos ofrecen una forma más robusta y fiable de manejar HTML.

## Ver también:
- [pup](https://github.com/ericchiang/pup): un procesador de HTML para la línea de comandos.
- [HTML-XML-utils](https://www.w3.org/Tools/HTML-XML-utils/): un conjunto de utilidades para procesar documentos HTML y XML.
- [tidy](http://www.html-tidy.org/): una herramienta para limpiar y formatear código HTML.
- Fish shell scripting tutorial: https://fishshell.com/docs/current/index.html#scripting