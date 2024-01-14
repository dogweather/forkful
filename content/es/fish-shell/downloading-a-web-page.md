---
title:                "Fish Shell: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué descargar una página web?

Descargar una página web puede ser útil para guardar una copia local de un sitio o para realizar análisis de datos. También puede ser útil para acceder a una página específica que está fuera de línea o para tener acceso a información durante una pérdida de conexión a Internet.

## Cómo hacerlo

Para descargar una página web en Fish Shell, podemos utilizar el comando `curl`. Este comando nos permite especificar la URL que queremos descargar y dónde queremos guardar el archivo.

```Fish Shell
curl -o destino.html https://www.ejemplo.com
```

Este comando guardará el archivo en un archivo llamado "destino.html" en el directorio actual. Si queremos guardar el archivo en un directorio específico, podemos especificar la ruta completa en lugar de solo el nombre del archivo.

También podemos usar `wget` para descargar una página web en Fish Shell, que funciona de manera similar a `curl`. Sin embargo, `wget` nos permite descargar páginas web recursivamente, lo que significa que también descargará todos los enlaces y recursos contenidos en la página.

```Fish Shell
wget -r -p destino https://www.ejemplo.com
```

Este comando descargará la página web y todos sus enlaces y recursos en un directorio llamado "destino" en el directorio actual.

## Profundizando

Además de especificar la URL y la ubicación del archivo, podemos utilizar otras opciones con `curl` y `wget` para personalizar nuestra descarga de la página web. Por ejemplo, podemos utilizar diferentes opciones de autenticación, descargar solo ciertos tipos de archivos o limitar la velocidad de descarga.

También podemos utilizar herramientas externas como `pup` para extraer información específica de la página web descargada o `grep` para buscar ciertos patrones de texto en el archivo descargado.

## Ver también

- [Documentación oficial de Fish Shell para `curl`](https://fishshell.com/docs/current/commands.html#curl)
- [Cómo utilizar `wget` en Fish Shell](https://www.cyberciti.biz/howto/question/linux/wget-silent-download-script-examples.php)
- [Tutorial sobre cómo utilizar `pup` para realizar extracciones en HTML](https://github.com/EricChiang/pup)
- [Documentación de Fish Shell para `grep`](https://fishshell.com/docs/current/commands.html#grep)