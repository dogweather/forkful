---
title:                "Analizando HTML"
aliases: - /es/clojure/parsing-html.md
date:                  2024-02-03T19:11:31.405398-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Analizar HTML en Clojure implica extraer información de documentos HTML de manera programática. Los programadores hacen esto para acceder, manipular o monitorear el contenido web de manera dinámica, automatizando tareas o alimentando datos a aplicaciones.

## Cómo hacerlo:

Clojure no tiene capacidades de análisis de HTML incorporadas, pero puedes usar bibliotecas de Java o envoltorios de Clojure como `enlive` o `hickory`. Aquí te muestro cómo usar ambos:

### Usando Enlive:

Enlive es una opción popular para el análisis de HTML y la obtención de datos web. Primero, inclúyelo en las dependencias de tu proyecto:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Luego, puedes analizar y navegar por HTML de la siguiente manera:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Este fragmento obtiene una página HTML y selecciona todos los elementos `<div>` con la clase `some-class`.

La salida puede parecerse a:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Aquí hay algo de contenido."]})
```

### Usando Hickory:

Hickory ofrece una manera de analizar HTML a un formato que es más fácil de manejar en Clojure. Agrega Hickory a las dependencias de tu proyecto:

```clojure
[hickory "0.7.1"]
```

Aquí tienes un ejemplo simple:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Analizar el HTML al formato de Hickory
(let [doc (hickory/parse "<html><body><div id='main'>¡Hola, mundo!</div></body></html>")]
  ;; Seleccionar el div con id 'main'
  (select/select (select/id "main") doc))
```

Este código analiza una simple cadena HTML y utiliza un selector CSS para encontrar un `div` con el ID `main`.

Resultado de muestra:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["¡Hola, mundo!"]}]
```

Tanto `enlive` como `hickory` ofrecen soluciones robustas para el análisis de HTML en Clojure, con `enlive` enfocándose más en la plantilla y `hickory` enfatizando la transformación de datos.
