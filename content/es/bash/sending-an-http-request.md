---
title:                "Enviando una solicitud http"
date:                  2024-01-20T17:59:11.596561-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Enviar una solicitud HTTP permite a tu script interactuar con servicios web, obtener datos o invocar operaciones remotas. Los programadores hacen esto para integrar aplicaciones, automatizar tareas o acceder a APIs.

## Cómo:
```Bash
# Usar cURL para enviar una solicitud GET simple
curl http://miapi.com/datos

# Salida esperada:
# {"nombre":"Juan","edad":30}

# Enviar una solicitud POST con datos
curl -d "param1=valor1&param2=valor2" -X POST http://miapi.com/endpoint

# Salida esperada:
# {"respuesta":"Datos recibidos"}

# Establecer encabezados para una solicitud
curl -H "Content-Type: application/json" -X POST -d '{"usuario":"Ana","clave":"secreto"}' http://miapi.com/sesion

# Salida esperada:
# {"estado":"Sesión iniciada"}
```

## Detalles a Fondo:
Muchas herramientas han permitido hacer solicitudes HTTP desde la línea de comandos, pero cURL ha sobresalido desde su creación en 1997. Es ampliamente usado por su portabilidad y fácil de usar para hacer solicitudes GET, POST, y otras operaciones HTTP.

Alternativas como `wget` son comunes para descargas, mientras que herramientas más recientes como `HTTPie` buscan ofrecer una experiencia más amigable y moderna. En escenarios más avanzados, puedes usar scripts en lenguajes como Python o Node.js que ofrecen librerías más robustas para manejar HTTP.

La implementación de solicitudes HTTP en Bash generalmente implica cURL o wget. Al usar cURL, puedes controlar métodos, encabezados, cuerpo de la solicitud y más. La respuesta del servidor se puede procesar con herramientas como `jq` para JSON en Bash.

## Ver También:
- Documentación de cURL: [https://curl.se/docs/manpage.html](https://curl.se/docs/manpage.html)
- Tutorial de cURL para principiantes: [https://linuxize.com/post/curl-rest-api/](https://linuxize.com/post/curl-rest-api/)
- Introducción a HTTPie: [https://httpie.io/docs#introduction](https://httpie.io/docs#introduction)
- Uso de `jq` para procesar JSON: [https://stedolan.github.io/jq/tutorial/](https://stedolan.github.io/jq/tutorial/)
