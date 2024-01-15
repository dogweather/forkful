---
title:                "Analizando html"
html_title:           "Bash: Analizando html"
simple_title:         "Analizando html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si alguna vez has navegado por internet, es muy probable que te hayas encontrado con páginas web cuyas estructuras y contenidos están escritos en lenguaje HTML. Aunque visualmente puedan ser atractivas, a veces necesitamos extraer información específica de estas páginas, ya sea para uso personal o para diferentes propósitos. Aquí es donde entra en juego el parsing de HTML en Bash, ya que nos permite analizar y extraer datos de estas páginas web de manera sencilla.

## Cómo hacerlo

El parsing de HTML en Bash puede ser realizado con diferentes herramientas, pero en este artículo nos enfocaremos en la librería `html-xml-utils`. Para empezar, debes asegurarte de tener instalada esta librería en tu sistema. Luego, puedes seguir los siguientes pasos:

1. Descarga el código fuente de la página web que deseas analizar, por ejemplo `https://www.example.com/`.

```Bash
curl https://www.example.com/ > webpage.html 
```

2. Utilizando el comando `hxnormalize` de `html-xml-utils`, limpiamos y normalizamos el código HTML de la página web descargada.

```Bash
hxnormalize -l 100 webpage.html > clean_webpage.html
```

3. Ahora, podemos utilizar el comando `hxselect` para filtrar y extraer las etiquetas y elementos específicos que deseamos de la página web. Por ejemplo, si queremos obtener el título de la página, podemos indicarle a `hxselect` que busque la etiqueta `title` dentro del archivo HTML limpio.

```Bash
hxselect -c 'title' clean_webpage.html 
```

4. También es posible realizar búsquedas más específicas utilizando selectores de CSS. Por ejemplo, si queremos obtener todos los enlaces de una determinada sección de la página web, podemos utilizar el siguiente comando:

```Bash
hxselect -c 'div.section a' clean_webpage.html 
```

## Deep Dive

El comando `hxselect` utiliza selectores de CSS para filtrar y extraer información de una página HTML. Estos selectores son similares a los que se usan en lenguajes de estilo como CSS y pueden ser muy útiles para realizar búsquedas más precisas. Además, la librería `html-xml-utils` cuenta con otras herramientas como `hxtoxml` y `hxwls` que nos permiten convertir archivos HTML a XML y realizar operaciones en ellos de manera más avanzada.

## Ver también

- [Documentación de `html-xml-utils`](https://www.w3.org/Tools/HTML-XML-utils/)
- [Artículo sobre parsing de HTML con Bash en SitePoint (en inglés)](https://www.sitepoint.com/essential-command-line-tools-for-html-xml-and-xhtml/)