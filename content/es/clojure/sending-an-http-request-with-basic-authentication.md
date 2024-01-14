---
title:                "Clojure: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma segura de proteger la información confidencial al enviar o recibir datos a través de una conexión HTTP. Esto garantiza que solo los usuarios autorizados puedan acceder a los datos y ayuda a prevenir posibles ataques.

## Cómo hacerlo

```Clojure
(ns mi-proyecto.core
  (:require [clj-http.client :as http]))

;; Definir las credenciales de autenticación
(def credenciales {:basic-auth ["usuario" "contraseña"]})

;; Enviar una solicitud GET con autenticación básica
(http/get "https://ejemplo.com/api/datos"
          {:basic-auth ["usuario" "contraseña"]})
```

El código anterior utiliza la librería "clj-http" para establecer y enviar una solicitud HTTP con autenticación básica. Primero, definimos las credenciales de autenticación en un mapa y luego las incluimos en la solicitud HTTP con el parámetro `:basic-auth`. Esto garantiza que la solicitud se realice con las credenciales correctas y pueda acceder a los datos de forma segura.

La salida del código anterior será un mapa con la respuesta de la solicitud, que puede ser procesada y utilizada como sea necesario.

## Profundizando en la autenticación básica HTTP

La autenticación básica HTTP es un método sencillo de autenticación que se basa en el envío de un nombre de usuario y una contraseña en texto plano a través de una conexión HTTP. Sin embargo, esta forma de autenticación no es segura ya que la información se envía sin cifrar, lo que la hace vulnerable a posibles ataques.

Para mejorar la seguridad, se puede utilizar una conexión HTTPS en lugar de HTTP. Esto asegura que los datos se transmitan encriptados, lo que dificulta que un atacante acceda a la información de la autenticación básica.

Además, es importante utilizar contraseñas seguras y cambiarlas regularmente para una mayor protección. También se recomienda utilizar otros métodos de autenticación más seguros, como OAuth, en lugar de la autenticación básica.

## Ver también

- [Documentación oficial de clj-http](https://github.com/dakrone/clj-http)
- [Más información sobre la autenticación básica HTTP](https://www.w3.org/Protocols/HTTP/Authentication.html)
- [Tutorial de autenticación básica en Clojure](https://spin.atomicobject.com/2017/03/30/basic-http-authentication-clojure/)