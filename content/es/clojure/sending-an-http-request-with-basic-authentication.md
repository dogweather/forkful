---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

En programación, enviar una solicitud HTTP con autenticación básica es equivalente a autenticar el acceso a un servidor web. Los programadores hacen esto para proteger los datos, asegurando que sólo las partes autorizadas los pueden ver.

## Cómo se hace:

Para enviar una solicitud HTTP con autenticación básica en Clojure, podemos hacer uso de la biblioteca de clj-http.

```Clojure
(require '[clj-http.client :as client])

(defn send-request
  []
  (client/get "http://ejemplo.com" {:basic-auth ["usuario" "contraseña"]}))
```

La respuesta será algo como esto:

```Clojure
{:status 200, :headers {"Content-Type" "text/html"}, :body ...}
```

## Buceo profundo

Históricamente, la autenticación básica es uno de los esquemas de autenticación más simples desarrollados como parte del protocolo HTTP. Sin embargo, es importante tener en cuenta que no ofrece una seguridad completa y debe utilizarse en conjunto con otros protocolos de seguridad, como HTTPS.

Como alternativas para enviar solicitudes HTTP con autenticación básica en Clojure, podríamos considerar otras bibliotecas como http-kit o aleph, pero clj-http es la más popular debido a su amplio soporte y facilidad de uso.

La implementación de la solicitud HTTP con autenticación básica en clj-http es bastante directa y su código está bien documentado. Consiste en construir una solicitud HTTP y agregar la cabecera de autenticación. La cabecera de autenticación es una codificación Base64 de la cadena "usuario:contraseña".

## Ver también

Para más información, puedes consultar las siguientes fuentes:

- Documentación de clj-http: https://github.com/dakrone/clj-http
- Especificación de autenticación básica HTTP: https://tools.ietf.org/html/rfc7617
- Biblioteca alternativa http-kit: http://www.http-kit.org/
- Biblioteca alternativa aleph: https://github.com/ztellman/aleph