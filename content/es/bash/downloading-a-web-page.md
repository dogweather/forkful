---
title:                "Descargando una página web"
html_title:           "Bash: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web usando Bash puede ser muy útil para muchas situaciones. Por ejemplo, puedes automatizar la descarga de archivos o realizar scraping de datos sin necesidad de un navegador. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo hacerlo

Para descargar una página web usando Bash, puedes utilizar el comando `wget`. Este comando permite descargar archivos y páginas web desde una URL. Aquí tienes un ejemplo de cómo descargar una página web desde la terminal:

```Bash
wget https://www.example.com
```

Esto descargará el contenido de la página web y lo guardará en un archivo llamado `index.html` en el directorio actual. Si quieres guardar el archivo con otro nombre, puedes usar la opción `-O`:

```Bash
wget https://www.example.com -O mi_pagina.html
```

También puedes especificar dónde quieres guardar el archivo con la opción `-P`:

```Bash
wget https://www.example.com -P /ruta/de/guardado/
```

Si solo quieres descargar ciertos tipos de archivos, puedes usar la opción `-A` seguida de una lista de extensiones separadas por comas. Por ejemplo, si solo quieres descargar imágenes y archivos PDF, puedes hacer lo siguiente:

```Bash
wget https://www.example.com -A jpg,pdf
```

Además, puedes usar la opción `-r` para descargar páginas web recursivamente, es decir, descargar todas las páginas que están enlazadas dentro de la página inicial.

## Profundizando

El comando `wget` es muy versátil y cuenta con una gran cantidad de opciones que puedes utilizar en tus descargas de páginas web. Puedes consultar la documentación oficial o usar el comando `wget --help` para obtener más información.

Además, si quieres realizar scraping de datos o automatizar descargas de páginas web de forma más avanzada, puedes usar Bash en conjunto con otras herramientas como `grep` y `sed` para filtrar y manipular el contenido descargado.

¡Con estos conocimientos ya estás listo para descargar páginas web usando Bash! Pero recuerda siempre tener cuidado al descargar contenido de internet, ¡no vayas a descargar nada malicioso!

## Ver también

- Documentación oficial de `wget`: https://www.gnu.org/software/wget/
- Tutorial interactivo de Bash en Codeacademy: https://www.codecademy.com/learn/learn-bash