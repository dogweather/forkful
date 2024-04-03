---
date: 2024-01-20 17:59:43.511164-07:00
description: "C\xF3mo hacerlo: En Fish, puedes usar `curl` o `wget` para enviar solicitudes\
  \ HTTP. Aqu\xED hay ejemplos r\xE1pidos para obtener el contenido de example.com."
lastmod: '2024-03-13T22:44:59.498618-06:00'
model: gpt-4-1106-preview
summary: En Fish, puedes usar `curl` o `wget` para enviar solicitudes HTTP.
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
En Fish, puedes usar `curl` o `wget` para enviar solicitudes HTTP. Aquí hay ejemplos rápidos para obtener el contenido de example.com.

```Fish Shell
curl http://example.com
```

```Fish Shell
wget -qO- http://example.com
```

La salida será el HTML de la página de inicio de example.com.

## Deep Dive
Antes, los comandos como `telnet` se usaban para interactuar con servidores web, pero eran más complicados y menos seguros. `curl` y `wget` son herramientas especializadas que soportan múltiples protocolos y métodos de encriptación. `curl` es útil para probar y depurar APIs porque te permite enviar diferentes tipos de datos y personalizar cabeceras. `wget` es excelente para descargar archivos o sitios enteros. En Fish, usar estas herramientas es sencillo, pero la potencia reside en las opciones y argumentos que permiten una amplia personalización de las solicitudes.

## Ver También
- La documentación oficial de `curl`: https://curl.se/docs/
- La documentación oficial de `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Tutorial Fish Shell para principiantes: https://fishshell.com/docs/current/tutorial.html
