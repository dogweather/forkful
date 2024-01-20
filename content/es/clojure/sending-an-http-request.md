---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviar una solicitud HTTP en Clojure: Una guía introductoria

## ¿Qué y Por Qué?

Enviar una solicitud HTTP es el proceso de transmitir datos y solicitar recursos a un servidor a través del protocolo HTTP. Los programadores lo hacen para interactuar con la web, acceder a APIs y realizar tareas basadas en la red.

## Cómo hacer:

Clojure nos proporciona las dependencias `clj-http` para manejar las solicitudes HTTP fácilmente. Aquí tienes un ejemplo de cómo enviar una solicitud GET con `clj-http`.

```Clojure
(require '[clj-http.client :as client])

(def response (client/get "http://example.com" 
    {:headers {"Accept" "application/json"}}))

(println (:status response))
(println (:body response))
```

Este código recupera una página web (http://example.com) y muestra la respuesta, incluyendo el estado y el contenido de la página.

## Inmersión Profunda

La capacidad de enviar solicitudes HTTP se incluyó en los primeros días de Clojure, reconociendo la creciente importancia de las comunicaciones basadas en web en la programación moderna. Una alternativa popular a `clj-http` es `http-kit`, que puede ser más adecuada para situaciones de alto rendimiento debido a su modelo de threading asíncrono. 

Cuando enviamos una solicitud HTTP con `clj-http`, se crea una nueva conexión con cada solicitud bajo el capó, lo cual puede no ser eficiente en aplicaciones de alto rendimiento.

## Ver También

Para explorar más sobre el trabajo con HTTP en Clojure, te recomiendo estos recursos:

- La documentación oficial de `clj-http`: https://github.com/dakrone/clj-http
- Un artículo útil en la programación Clojure HTTP: https://www.toptal.com/clojure/clojure-http-client-clj-http
- `http-kit`, una alternativa a `clj-http`: http://www.http-kit.org/379.html

No dudes en experimentar y elegir la herramienta que mejor se adapte a tus necesidades de programación.