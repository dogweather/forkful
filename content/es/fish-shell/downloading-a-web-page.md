---
title:                "Descargando una página web"
html_title:           "Fish Shell: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué? 
Descargar una página web es un proceso en el que se obtiene el contenido de una página de Internet en formato HTML. Los programadores suelen hacerlo para acceder a la información de una página y procesarla, ya sea para mostrarla en una aplicación o para realizar análisis de datos.

# Cómo:
```Fish Shell``` tiene una herramienta llamada ```curl``` que permite descargar páginas web. Para descargar una página, simplemente escribimos ```curl [url]``` en la terminal de Fish Shell y el contenido de la página se mostrará en la pantalla. También podemos guardar el contenido en un archivo usando el parámetro ```-o [nombre de archivo]```, por ejemplo: 
```Fish Shell
curl -o pagina.html [url]
```
Esto creará un archivo llamado "pagina.html" en la misma carpeta donde estemos ejecutando el comando, que contendrá el contenido de la página web.

# Deep Dive:
Antes de que existiera ```curl```, los programadores tenían que usar herramientas como ```wget``` o ```fetch``` para descargar páginas web. Sin embargo, ```curl``` es una herramienta más versátil y potente, ya que permite realizar diferentes tipos de solicitudes HTTP y trabajar con diferentes protocolos.

Si bien descargar páginas web puede ser útil en ciertas situaciones, es importante tener en cuenta que esto puede ser considerado como una actividad maliciosa si se hace en exceso o sin permiso del propietario de la página. Por lo tanto, siempre es importante tener en cuenta la ética y las políticas de uso de una página antes de descargar su contenido.

# See Also:
- Documentación oficial de ```curl```: https://fishshell.com/docs/current/commands.html#curl
- Tutorial de ```curl```: https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-download-files-from-the-command-line
- Alternativas a ```curl```: https://www.tecmint.com/curl-alternatives-a-powerful-web-client-tool/