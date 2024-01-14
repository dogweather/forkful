---
title:                "Clojure: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web puede ser útil para obtener información o datos de una página específica. Además, puede ser una técnica útil para automatizar ciertas tareas en línea.

## Cómo hacerlo

En Clojure, podemos utilizar la biblioteca `clj-http` para descargar una página web. Primero, debemos importar la biblioteca en nuestro proyecto:

```Clojure
(ns mi-proyecto.core
  (:require [clj-http.client :as client]))
```

Luego, podemos utilizar la función `client/get` para descargar la página. Por ejemplo, para descargar la página de Google, podemos hacer lo siguiente:

```Clojure
(client/get "http://www.google.com")
```

Esto nos devolverá un mapa con información sobre la respuesta, incluyendo el código de estado, encabezados y el contenido de la página.

Una vez que tenemos la página descargada, podemos extraer la información que necesitamos utilizando herramientas como `clojure.xml` o `clojure.data.json` dependiendo del formato de la página.

## Profundizando

Descargar una página web puede ser más complejo de lo que parece a simple vista. Hay muchas variables a considerar, como autorización, cookies, redireccionamiento, entre otros. Es importante investigar y tener en cuenta estos factores al desarrollar una solución para descargar páginas web en Clojure.

## Ver también

- [Documentación de `clj-http`](https://github.com/dakrone/clj-http)
- [Tutorial básico de Clojure](https://clojure.org/guides/getting_started)
- [Introducción a la programación funcional en Clojure](https://clojure.org/community/resources)