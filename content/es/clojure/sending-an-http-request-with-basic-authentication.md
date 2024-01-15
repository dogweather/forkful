---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Clojure: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Quizás estés preguntándote por qué alguien querría enviar una solicitud HTTP con autenticación básica. Bueno, la autenticación básica es una forma sencilla de proteger una API o una aplicación web, permitiendo el acceso solo a usuarios autorizados que puedan proporcionar credenciales válidas.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Clojure, hay algunos pasos sencillos que debes seguir. En primer lugar, necesitas importar la biblioteca `clj-http.client` para manejar las solicitudes HTTP. Luego, debes proporcionar las credenciales de autenticación en forma de mapa, con las claves `:basic-auth-username` y `:basic-auth-password`. A continuación, usa la función `http-basic-auth` para adjuntar las credenciales a la solicitud.

```Clojure
(ns my-app.core
  (:require [clj-http.client :as http]))

(defn make-request []
  (let [credentials {:basic-auth-username "usuario"
                     :basic-auth-password "contraseña"}
        request (http/http-basic-auth
                 (http/get "https://api.miapp.com/users")
                 credentials)]
    (http/https request)))
```

¡Eso es todo! Si las credenciales son correctas, la solicitud se realizará con éxito. Si no, recibirás un error de autenticación.

## Profundizando

Ahora que sabes cómo enviar una solicitud HTTP con autenticación básica, aquí hay algunos detalles adicionales a tener en cuenta. En primer lugar, ten en cuenta que las credenciales se proporcionan en texto plano en la solicitud. Por lo tanto, debes asegurarte de que el intercambio de información se realice a través de una conexión segura (HTTPS) para protegerlas.

Además, ten en cuenta que la autenticación básica es una forma muy simple de autenticarse y no ofrece tanta seguridad como otros métodos más complejos. Si necesitas una mayor seguridad, considera el uso de OAuth o tokens de acceso.

## Ver también
- [Documentación oficial de `clj-http`](https://github.com/dakrone/clj-http)
- [Artículo en Medium sobre autenticación básica en Clojure](https://medium.com/@adnankiani/basic-authentication-in-clojure-using-clj-http-c8dd050d63ff)
- [Ejemplo de código en GitHub sobre envío de solicitudes HTTP con autenticación básica en Clojure](https://github.com/mikethompson/clojure-http-client/blob/master/src/clojure_http_client/examples/basic_auth.clj)