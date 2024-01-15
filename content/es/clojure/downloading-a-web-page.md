---
title:                "Descargando una página web."
html_title:           "Clojure: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado descargar una página web completa para consultarla sin conexión? O tal vez quieres extraer información específica de una página web para usarla en tu propio programa. Sea cual sea la razón, en este artículo aprenderás cómo descargar páginas web utilizando Clojure.

## Cómo hacerlo

El proceso de descarga de una página web con Clojure es bastante simple. Primero, necesitamos importar la librería `clj-http` que nos permitirá hacer solicitudes HTTP.

```Clojure
(require '[clj-http.client :as client])
```

Luego, utilizamos la función `get` de la librería para hacer una solicitud HTTP GET a la URL de la página web que queremos descargar. Por ejemplo, si queremos descargar la página principal de Google, podemos hacer lo siguiente:

```Clojure
(def page (client/get "https://www.google.com"))
```

La variable `page` ahora contiene toda la información de la página web descargada. Podemos acceder al código de estado de la respuesta, encabezados y cuerpo utilizando las siguientes funciones:

```Clojure
(:status page) ; código de estado de la respuesta
(:headers page) ; encabezados de la respuesta
(:body page) ; cuerpo de la respuesta
```

Por ejemplo, si queremos imprimir el código de estado de la respuesta de la página de Google, podemos hacer lo siguiente:

```Clojure
(println "Código de estado:" (:status page))
```

Esto imprimirá "Código de estado: 200" que significa que la descarga fue exitosa.

## Inmersión Profunda

Si queremos extraer información específica de la página web descargada, podemos usar la librería `clojure.xml` para convertir el cuerpo de la respuesta a un mapa y luego utilizar funciones de Clojure para acceder a los datos que necesitamos.

Por ejemplo, si queremos obtener todos los enlaces de la página web descargada, podemos utilizar la función `select` de la librería `clojure.xml` junto con la función `:href` para obtener los enlaces. El código se vería así:

```Clojure
(require '[clojure.xml :as xml])

(def page-map (xml/parse (:body page)))

(def links (xml/select page-map :a))

(def hrefs (map :href links))

(println "Enlaces encontrados:" hrefs)
```

Esto imprimirá todos los enlaces encontrados en la página web descargada.

## Ver también

- Documentación oficial de `clj-http`: https://clj-http.github.io/
- Tutorial de Clojure para principiantes: https://clojure.org/guides/repl/introduction
- Librería `clojure.xml`: https://clojure.github.io/clojure/clojure.xml-api.html