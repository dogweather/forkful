---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:01:15.950659-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica significa mandar información al servidor junto con un usuario y contraseña codificados. Los programadores lo hacen para acceder a recursos que requieren identificación, asegurando un nivel básico de seguridad.

## Cómo hacerlo:

```Clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url user password]
  (let [credentials (str user ":" password)
        encoded-creds (-> credentials (.getBytes) java.util.Base64/getEncoder (.encodeToString))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-creds)}})))

;; Uso:
(println (fetch-protected-resource "http://tu-api.com/recurso-protegido" "usuario" "contraseña"))
```

Sample output (simulation):
```Clojure
{:status 200, :headers {...}, :body "..."}
```

## Profundización:

La autenticación básica es un método clásico que se remonta a los primeros días de la web. No es el más seguro, puesto que depende de HTTPS para proteger las credenciales en tránsito. Si buscas mayor seguridad, considera usar tokens de autenticación, OAuth o JWT (JSON Web Tokens).

Cuando usas Clojure para enviar solicitudes HTTP, puedes elegir entre `clj-http` y otras bibliotecas como `http-kit` o `aleph`. Cada una tiene sus peculiaridades, pero `clj-http` es conocida por su simplicidad y rica interfaz.

En la ejecución del código, asegúrate de que las credenciales no estén codificadas en el mismo. Utiliza variables de entorno o sistemas de gestión de configuración para mantener la seguridad.

## Ver También:

- [clj-http documentation](https://github.com/dakrone/clj-http)
- [The Clojure Toolbox - Lista de bibliotecas de Clojure](https://www.clojure-toolbox.com/)
- [RFC 7617 - La autenticación básica en HTTP](https://tools.ietf.org/html/rfc7617)
- [Clojure.org - Guía de inicio rápido](https://clojure.org/guides/getting_started)
