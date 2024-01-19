---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando una solicitud HTTP con autenticación básica en Bash 

## ¿Qué y Por Qué?

En ocasiones, un programa debe pedir datos de una web que requiere identificación. El envío de una solicitud HTTP con autenticación básica en Bash permite al programa identificarse.

## Cómo hacerlo:

```Bash
usuario="user123"
password="pass123"

url="http://miweb.com"

# Utilizamos curl para enviar la solicitud
respuesta=$(curl -s -u "$usuario:$password" "$url")

# Imprimimos la respuesta
echo "$respuesta"
```

La ejecución mostrará la respuesta del server a tu solicitud.

## Inmersión Profunda:

Este enfoque viene desde los días tempranos de la web cuando la autenticación básica era el estándar en HTTP. Aunque es menos seguro que otras opciones como la autenticación compleja (digest authentication) o los tokens OAuth2, sigue siendo útil por ser simple y directo.

Alternativas podrían ser `wget` en lugar de `curl`. También puedes usar otros lenguajes de alto nivel como Python o Java, que tienen bibliotecas ricas para HTTP.

Este script en bash usa `curl`, una herramienta potente para transferir datos desde o hacia un servidor. Con la opción `-u`, añades las credenciales necesarias para la autenticación básica.

## Ver También:

Para más información, consulta las siguientes fuentes:

- `curl`: https://curl.se/
- Autenticación HTTP básica: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication
- `wget`: https://www.gnu.org/software/wget/
- Autenticación digest: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#digest_authentication
- OAuth2: https://oauth.net/2/