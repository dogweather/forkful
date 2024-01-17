---
title:                "Envío de una solicitud http con autenticación básica"
html_title:           "Clojure: Envío de una solicitud http con autenticación básica"
simple_title:         "Envío de una solicitud http con autenticación básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es simplemente incluir un nombre de usuario y una contraseña en la solicitud para acceder a recursos protegidos. Los programadores lo hacen para asegurarse de que solo los usuarios autorizados puedan acceder a ciertas funciones o información en sus aplicaciones.

## Cómo:
```Clojure
(require '[clj-http.client :as client]) ; importar la biblioteca clj-http

; realizar una solicitud GET con autenticación básica
(def response (client/get "https://ejemplo.com" :basic-auth "usuario" "contraseña"))

; realizar una solicitud POST con autenticación básica y enviar datos
(def response (client/post "https://ejemplo.com" {:basic-auth ["usuario" "contraseña"] :form-params {"dato1" "valor1" "dato2" "valor2"}}))
```

## Profundizando:
La autenticación básica es un método de autenticación simple y común utilizado en aplicaciones web. Fue introducido en HTTP desde su versión 1.0 y sigue siendo ampliamente utilizado hoy en día. Sin embargo, algunos programadores prefieren utilizar otras formas de autenticación más seguras, como OAuth.

Para implementar la autenticación básica en Clojure, podemos utilizar la biblioteca clj-http o incluso escribir nuestra propia función de autenticación. Al enviar una solicitud con autenticación básica, es importante asegurarse de que la conexión sea segura, por lo que se recomienda utilizar HTTPS en lugar de HTTP.

## Ver también:
- [Documentación de clj-http](https://github.com/dakrone/clj-http)
- [Tutorial de autenticación básica en Clojure](https://medium.com/@helena_53282/basic-authentication-in-a-clojure-api-application-30a6e05cfec0)
- [Comparación entre autenticación básica y OAuth](https://medium.com/@darutk/basics-of-authentication-with-rest-api-443665ef76bd)