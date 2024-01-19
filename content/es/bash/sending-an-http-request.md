---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es como hacer una petición a un servidor web para acceder a su información. Los programadores lo hacen para interactuar con APIs, recuperar datos de sitios web, y más.

## ¿Cómo hacerlo?

Aquí hay un ejemplo de cómo puedes enviar una solicitud HTTP usando cURL en Bash. 

```Bash
curl http://api.misitio.com/endpoint 
```

Y si quieres enviar una solicitud POST con un cuerpo de JSON:

```Bash
curl -X POST -d '{"clave":"valor"}' -H "Content-Type: application/json" http://api.misitio.com/endpoint
```

La respuesta del servidor se imprimirá en la terminal.

## Profundizando

cURL lleva existiendo desde 1996 y es uno de los métodos más antiguos para interactuar con HTTP mediante la terminal. Sin embargo, hay otras alternativas, como wget, httpie, etc.

En términos de detalles de implementación, cuando envías una solicitud HTTP, en realidad estás enviando un paquete de datos al servidor que incluye detalles como el método HTTP (GET, POST, etc.), las cabeceras, y un cuerpo de la solicitud (si es necesario).

## Ver también

Aquí hay algunos enlaces a recursos relacionados que podrían ser útiles:

- Documentación oficial de cURL: https://curl.se/docs/
- Guía de uso de cURL: https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-download-files-and-webpages-from-the-command-line
- Algunas alternativas a cURL: https://www.keycdn.com/blog/http-requests