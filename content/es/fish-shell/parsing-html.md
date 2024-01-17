---
title:                "Analizando html"
html_title:           "Fish Shell: Analizando html"
simple_title:         "Analizando html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
El parsing de HTML es el proceso de analizar y manipular el código HTML de una página web. Los programadores lo hacen para extraer información específica de una página, como el texto de un artículo o los enlaces de una sidebar, y utilizarla en su propio código.

## ¡Vamos a ello!
¡Aprender cómo hacer parsing de HTML usando el Fish Shell es muy sencillo! Primero, necesitamos instalar la herramienta "pup" usando la función "set -U" en nuestro archivo config.fish:
```
set -U fish_user_paths /usr/local/bin $fish_user_paths
```
Luego, podemos utilizar pup para seleccionar elementos específicos del HTML utilizando selectores CSS:
```
curl -s https://www.example.com | pup 'a.attr{href}'
```
Este comando nos devolverá una lista de todos los enlaces en la página web con el atributo href. ¡Así de fácil!

## Profundizando
El parsing de HTML ha sido una técnica ampliamente utilizada desde los primeros días de la web. Antes de herramientas como pup, los programadores tenían que escribir su propio código para analizar y extraer datos de páginas web. Aunque existen alternativas como BeautifulSoup en Python, muchos programadores prefieren utilizar el poderoso y simplificado Fish Shell.

## ¡Echa un vistazo!
Si quieres profundizar más en el tema del parsing de HTML con Fish Shell, te recomendamos revisar el repositorio de GitHub de pup, donde encontrarás más información y ejemplos de cómo utilizar esta herramienta. ¡Aprenderás a realizar tareas aún más complejas utilizando esta técnica!