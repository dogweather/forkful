---
title:                "Enviando una solicitud http"
aliases:
- /es/clojure/sending-an-http-request/
date:                  2024-01-20T17:59:24.066585-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Enviar una solicitud HTTP es la forma de pedir datos o realizar una acción en un servidor web. Los programadores lo hacen para interactuar con APIs, servicios web o cualquier recurso disponible en internet.

## Cómo hacerlo:
Clojure ofrece varias bibliotecas para realizar solicitudes HTTP. Vamos a usar `clj-http` por su simplicidad y popularidad.

```clojure
; Asegúrate de tener clj-http añadido a tu proyecto
(require '[clj-http.client :as client])

; Enviar una simple solicitud GET
(def respuesta-get (client/get "https://jsonplaceholder.typicode.com/posts/1"))
(println respuesta-get)

; Enviar una solicitud POST con datos
(def respuesta-post (client/post "https://jsonplaceholder.typicode.com/posts"
                  {:body (json/write-str {:title "Hola" :body "Mundo" :userId 1})
                   :headers {"Content-Type" "application/json"}}))
(println respuesta-post)
```

Salida de ejemplo para la solicitud GET:
```clojure
{:status 200, :headers {...}, :body "..."}
```
Salida de ejemplo para la solicitud POST:
```clojure
{:status 201, :headers {...}, :body "..."}
```

## Deep Dive:
Historialmente, las solicitudes HTTP en Clojure se han hecho con diversas bibliotecas, pero `clj-http` se ha convertido en el estándar de facto por su interfaz directa y conjunto completo de características.

Alternativas:
- `http-kit`: un cliente y servidor HTTP para Clojure que destaca por su rendimiento.
- `aleph`: utiliza la biblioteca Netty y es conocido por su modelo asíncrono.

Detalles de implementación:
- `clj-http` usa Java Interop bajo el capó para hacer uso de las bibliotecas de HTTP de Java, combinando la facilidad de Clojure con la solidez del ecosistema Java.
- Las solicitudes se pueden personalizar extensamente, permitiendo ajustar parámetros como tiempos de espera, manejo de redirecciones, y adjuntar datos en varias formas.

## Ver también:
- Documentación de `clj-http`: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Guía de inicio rápido para Clojure: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- JSONPlaceholder, una API para pruebas: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)
