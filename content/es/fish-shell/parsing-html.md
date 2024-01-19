---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Parsear HTML se trata de desglosar y entender el contenido de una página web. Los programadores lo hacen para extraer información específica y utilizarla en sus propios programas.

## Cómo se hace:

La Fish Shell puede ayudarte con esto. Asegúrate de tener instalado Fish (versión actual) y `htmlparser`. Aquí un ejemplo para parsear el título de una página web.

```Fish Shell
function parsear_titulo -d "Parsea el título de una página HTML"
    set pagina $argv[1]
    curl -s $pagina | htmlparser select ".title"
end
```

Lo usamos como:

```Fish Shell
> parsear_titulo "http://miweb.com"
> "El título de tu página"
```

## Inmersión Profunda

Parsear HTML ha existido desde la creación del HTML en 1990. Aunque existen alternativas a Fish Shell para hacer esto, como Python o JavaScript, Fish Shell es conveniente debido a su simplicidad y fácil instalación.

Hablando de detalles de implementación, el comando `curl` en nuestro script descarga el HTML de la página. `htmlparser select` a continuación selecciona el contenido dentro de la etiqueta especificada - en este caso, ".title".

## Ver También

Para más información, consulta las siguientes fuentes:

- Documentación completa de Fish Shell: https://fishshell.com/docs/current/index.html
- La página man de htmlparser: https://manpages.ubuntu.com/manpages/bionic/man1/htmlparser.1.html
- Tutorial de Python para parsear HTML: https://docs.python.org/3/library/html.parser.html
- Node.js y Cheerio para parsear HTML: https://cheerio.js.org