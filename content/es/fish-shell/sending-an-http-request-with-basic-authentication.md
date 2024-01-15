---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Fish Shell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Si eres un desarrollador web o alguien que trabaja con APIs, es esencial saber cómo enviar una solicitud HTTP con autenticación básica. Esto te permitirá acceder a recursos protegidos y realizar acciones autorizadas en un servidor remoto.

## How To

```Fish Shell
# Primero, importa el módulo `http` en tu script de Fish Shell
source /usr/local/share/fish/vendor_completions.d/http.fish

# Luego, establece las credenciales de autenticación como variables
set -xu username <tu_usuario>
set -xu password <tu_contraseña>

# Ahora, puedes enviar una solicitud GET con autenticación básica
http -a $username:$password GET https://api.example.com/resources

# Para una solicitud POST, puedes incluir los datos en el cuerpo del mensaje
set -xu data '{"title": "Nuevo recurso", "description": "Esto es un nuevo recurso de ejemplo"}'
http -a $username:$password -f POST https://api.example.com/resources <<< $data
```

Output:

```
HTTP/1.1 200 OK
Date: Thu, 17 Sep 2020 18:46:22 GMT

{
  "id": "12345",
  "title": "Nuevo recurso",
  "description": "Esto es un nuevo recurso de ejemplo"
}
```

## Deep Dive

La autenticación básica es un método simple de autenticación que se basa en credenciales de usuario y contraseña. Esta información se envía al servidor en formato de texto sin cifrar, por lo que no se recomienda para entornos de producción. Sin embargo, es útil para fines de prueba y desarrollo.

Además de los ejemplos mencionados anteriormente, también puedes enviar solicitudes con otros métodos HTTP, como PUT, PATCH o DELETE, simplemente cambiando `GET` o `POST` en el comando `http` a tu método deseado. También puedes especificar encabezados adicionales en la solicitud mediante el uso del parámetro `-h`.

Para obtener más detalles sobre el uso de `http` en Fish Shell, consulta la documentación oficial [aquí](https://fishshell.com/docs/current/cmds/http.html).

## See Also

- [Comandos básicos de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Ejemplos de uso de autenticación básica con cURL](https://gist.github.com/shim-hyunseok/0db41e326627f7fa08af262e4ac2543c)