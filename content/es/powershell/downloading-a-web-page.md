---
title:                "Descargando una página web"
html_title:           "PowerShell: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¡Descarga páginas web con PowerShell!

Puede que hayas oído hablar de "descarga de páginas web" y te preguntes ¿qué significa eso exactamente? Básicamente, descargar una página web significa guardar su contenido en tu computadora desde internet. Los programadores lo hacen por varias razones, ya sea para analizar el contenido de la página, automatizar la descarga de archivos o simplemente para tener una copia de seguridad en caso de que la página se elimine en el futuro.

## Cómo:

Descargar una página web usando PowerShell es muy sencillo. Todo lo que necesitas es usar la función **Invoke-WebRequest** y proporcionar la URL de la página que deseas descargar. Por ejemplo:

```
PowerShell Invoke-WebRequest https://www.ejemplo.com
```

Esto descargará la página web y te mostrará una respuesta HTTP con el código de estado y el contenido de la página.

Puedes guardar el contenido de la página en un archivo usando el parámetro **OutFile**. Por ejemplo, si quieres guardar la página en un archivo llamado "pagina.html", puedes usar el siguiente comando:

```
PowerShell Invoke-WebRequest https://www.ejemplo.com -OutFile pagina.html
```

¡Y eso es todo! Ahora tienes una copia de la página web en tu computadora.

## Inmersión profunda:

La descarga de páginas web ha sido una técnica utilizada por los desarrolladores y programadores desde los primeros días de internet. Anteriormente, se usaban otras herramientas como **cURL** o **wget** para hacerlo, pero con PowerShell podemos hacerlo de manera más fácil y rápida.

Además de descargar páginas web completas, también puedes descargar archivos específicos de una página usando PowerShell. Por ejemplo, si quieres descargar todos los archivos PDF de un sitio web, puedes usar el comando *-Filter* con **Invoke-WebRequest** para especificar el tipo de archivo que deseas descargar.

## Ver también:

- [Documentación oficial de Microsoft sobre Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [Tutorial en vídeo sobre cómo descargar páginas web con PowerShell](https://www.youtube.com/watch?v=BCYhLEyNDYc)
- [Alternativa de descarga de páginas web utilizando otras herramientas de línea de comandos](https://www.lifewire.com/download-entire-website-command-line-3482184)