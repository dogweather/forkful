---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Enviar una solicitud HTTP con autenticación básica significa la transmisión de información de usuario y contraseña con todos los pedidos HTTP. Los programadores hacen esto para obtener acceso seguro a ciertos recursos en línea.

## Cómo hacerlo:

Aquí está una muestra de cómo hacerlo en Gleam:

```gleam
import gleam/http.{Request}

let request =
  Request.new(
    "https://example.com",
    [http.basic_auth("nombre_usuario", "contraseña")]
  )
```

Después de crear esta solicitud:
```gleam
import gleam/http.{send}

send(request)
```

Esto le dará algo así como la respuesta:
```gleam
Ok(
  Response(
    200,
    [],
    "¡Has autenticado exitosamente!",
  )
)
```

## Inmersión Profunda

1) Contexto Histórico: HTTP Basic Auth se introdujo con el estándar HTTP 1.0 en los años 90. Aunque es simple, tiene fallas de seguridad, ya que las contraseñas se envían como texto sin cifrar.
2) Alternativas: Puede usar Autenticación de portador de token, OAuth, o incluso una sesión basada en cookies, dependiendo de sus necesidades específicas.
3) Detalles de Implementación: En la autenticación básica, el encabezado `Authorization` contiene la palabra `Basic` seguida por un espacio y una cadena formada por el usuario y la contraseña codificada en Base64.

## Vea También

Si desea conocer más, estos enlaces pueden ser de utilidad:

- Documentación oficial de Gleam HTTP: https://hexdocs.pm/gleam_http/gleam/http/
- Autenticación Básica : https://developer.mozilla.org/es/docs/Web/HTTP/Headers/Authorization
- Comparación de métodos de autenticación: https://www.loginradius.com/engineering/blog/the-seven-types-of-rest-apis/