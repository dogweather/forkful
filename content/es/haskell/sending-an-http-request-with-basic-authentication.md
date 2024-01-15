---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Haskell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

¿Por qué alguien querría enviar una solicitud HTTP con autenticación básica? Puede haber muchas razones, pero en resumen, la autenticación básica es una forma simple y común de proteger una API o servicio web. Al enviar una solicitud con autenticación básica, el cliente debe proporcionar credenciales (nombre de usuario y contraseña) para verificar su identidad y tener acceso a los recursos protegidos.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Haskell, necesitaremos utilizar el módulo "Network.HTTP.Simple" de la biblioteca "http-conduit". Comencemos por importar el módulo y establecer nuestra URL de destino:

```Haskell
import Network.HTTP.Simple (httpLbs, parseRequest_, setRequestBasicAuth)
url = "https://www.example.com/users/123"
```

A continuación, analizamos la URL en un tipo de datos "Request" y le agregamos credenciales de autenticación básica con la función "setRequestBasicAuth":

```Haskell
request = parseRequest_ url
requestWithAuth = setRequestBasicAuth "username" "password" request
```

Finalmente, podemos enviar la solicitud con la función "httpLbs" y obtener la respuesta del servidor:

```Haskell
response = httpLbs requestWithAuth
```

Si la autenticación es exitosa, recibiremos una respuesta exitosa (código de estado 200) junto con los datos solicitados. Si la autenticación falla, obtendremos un error de autorización (código de estado 401).

## Profundizando

Puede parecer fácil enviar una solicitud con autenticación básica en Haskell, pero ¿qué está sucediendo bajo el capó? En realidad, la función "setRequestBasicAuth" está haciendo un poco más de trabajo por nosotros. Analicemos su definición:

```Haskell
setRequestBasicAuth :: ByteString -> ByteString -> Request -> Request
setRequestBasicAuth username password = setRequestHeader "Authorization" (basicAuthHeader username password)
```

Aquí, estamos agregando un encabezado "Authorization" a nuestra solicitud con una cadena codificada en base64 que contiene nuestras credenciales de autenticación. Esta cadena sigue el formato "username:password" y se agrega al encabezado en el siguiente formato:

```Haskell
"Basic " ++ (encodeBase64 (username ++ ":" ++ password))
```

Además, hay que tener en cuenta que la autenticación básica no es el método más seguro para proteger una API o servicio web, ya que la información de autenticación se pasa a través de la red en texto plano. Se recomienda utilizar métodos de autenticación más fuertes y seguros en su lugar.

## Ver también

- Documentación oficial de la biblioteca "http-conduit": https://hackage.haskell.org/package/http-conduit
- Ejemplo de autenticación básica en Haskell: https://www.snoyman.com/blog/2016/12/benchmarking-basic-auth
- Cómo implementar otros métodos de autenticación en Haskell: https://begriffs.com/posts/2017-01-14-designing-authentication-in-haskell.html