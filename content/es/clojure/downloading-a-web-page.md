---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Descargar una página web es el proceso de obtener y guardar localmente el contenido de un sitio web. Los programadores lo hacen para analizar o manipular sus datos, para monitorear cambios, entre otros usos.

## Cómo hacerlo:

La biblioteca `clj-http` en Clojure ofrece la función `get` para enviar solicitudes HTTP GET. Aquí hay un ejemplo:

```Clojure
(ns web-download.core
  (:require [clj-http.client :as client]))

(defn download-page [url]
  (client/get url {:as :string}))
```

Cuando ejecutes este código, te devolverá la respuesta HTTP del URL que has ingresado.

```Clojure
(download-page "https://www.some-web-page.com")
```

## Inmersión Profunda:

Clojure, un lenguaje de programación moderno basado en Lisp, ya incluye las opciones `slurp` o `clj-http` para descargar páginas web. Aunque `slurp` es más simple, `clj-http` ofrece mucha más flexibilidad y opciones (como agregar encabezados HTTP).

Hay otras alternativas para descargarse páginas web, como las funciones `wget` y `curl` en Linux o incluso las extensiones de navegador. La opción más adecuada depende de tu caso de uso.

Los detalles de implementación para descargar una página web en Clojure son simples: se envía un solicitud HTTP GET a la URL objetivo y luego se maneja la respuesta. Se puede almacenar la respuesta en una variable para su posterior análisis o manipulación.

## Ver También:

Aquí tienes algunas fuentes relacionadas para ampliar tus conocimientos:

- [Documentación oficial de clj-http](https://github.com/dakrone/clj-http)
- [Guía de HTTP en Clojure](https://www.clojure-toolbox.com/)