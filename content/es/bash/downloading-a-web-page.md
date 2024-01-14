---
title:                "Bash: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web puede ser una tarea muy útil para aquellos que quieren realizar un análisis de datos, extraer información específica o simplemente guardar una copia local de una página importante. Aprender a realizar esta tarea con Bash puede ahorrar tiempo y facilitar el proceso.

## Cómo hacerlo

Para descargar una página web utilizando Bash, necesitamos utilizar el comando `curl`. Este comando permite hacer solicitudes y recibir respuestas de páginas web en la línea de comandos. Veamos un ejemplo práctico:

```
# Descargar la página web de Wikipedia
curl https://es.wikipedia.org/wiki/Bash
```

Este comando nos permitirá descargar la página de Bash en español y guardarla en nuestra carpeta actual con el nombre de `index.html`.

Para guardar la página con un nombre específico, podemos utilizar la siguiente sintaxis:

```
# Descargar la página web de Google y guardarla con el nombre "google.html"
curl -o google.html https://www.google.com
```

También podemos descargar páginas de forma recursiva, lo que significa que se descargarán todas las páginas vinculadas a la página principal. Esto es útil para casos en los que queremos guardar una página completa con todas las subpáginas. Utilizamos el siguiente comando para realizar una descarga recursiva:

```
# Descargar de forma recursiva la página web de Github
curl -r -OJL https://github.com
```

Por último, si queremos extraer un archivo en particular de una página web, podemos utilizar el siguiente comando:

```
# Descargar el logo de Github
curl -O https://github.com/logos
```

¡Y eso es todo! Ahora tienes el archivo deseado guardado en tu carpeta local.

## Profundizando

Existen muchas más opciones y parámetros que podemos utilizar con el comando `curl` para personalizar aún más nuestras descargas. Por ejemplo, podemos establecer el usuario y la contraseña en el caso de que la página que queremos descargar esté protegida con autenticación. También podemos utilizar la opción `-X` para especificar un método HTTP específico como GET, POST o DELETE.

El comando `curl` también nos permite descargar utilizando diferentes protocolos, como FTP, HTTPS o SCP. Y para ver más detalles y estadísticas de nuestras descargas, podemos utilizar la opción `-i` para ver los encabezados de respuesta y `-v` para mostrar una mayor verbosidad.

## Véase también

- [Guía de comandos básicos de Bash](https://linuxhandbook.com/basic-bash-commands/)
- [Comandos útiles de Bash para programadores](https://www.linuxito.com/programacion/647-comandos-utiles-de-bash-para-programadores)
- [Documentación de la línea de comandos de Bash](https://ss64.com/bash/es/)