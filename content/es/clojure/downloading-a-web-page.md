---
title:                "Descargando una página web"
html_title:           "Clojure: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cómo descargar una página web usando Clojure

## Qué y por qué?
Descargar una página web es simplemente obtener el código HTML de una página desde su URL. Los programadores a menudo realizan esta acción para extraer información de una página web, automatizar tareas de navegación o crear una copia local de la página.

## Cómo hacerlo:
```
;; Primero, asegúrate de tener instalada la biblioteca clj-http:
[clojure.java io]
[clj-http.client :as http]
(def url "https://www.example.com")
;; Utilizaremos la función `get` para descargar el HTML de la página:
(let [resp (http/get url)]
  (println (:body resp)))
```
Este código imprimirá el código HTML de la página en la consola.

## Profundizando:
Descargar una página web se ha vuelto una tarea común en la era de la información. Existen otras opciones en Clojure para ello, como la biblioteca `net.cgrand.html`, que te permite navegar por el HTML con consultas CSS y extraer la información que necesites.

## Ver también:
- [Biblioteca clj-http](https://github.com/dakrone/clj-http)
- [Biblioteca net.cgrand.html](https://github.com/cgrand/enlive)