---
date: 2024-01-26 04:29:24.026085-07:00
description: "XML es un lenguaje de marcado para codificar documentos de una manera\
  \ que es legible tanto para humanos como para m\xE1quinas. Es clave en servicios\
  \ web,\u2026"
lastmod: '2024-03-13T22:44:58.683297-06:00'
model: gpt-4-0125-preview
summary: "XML es un lenguaje de marcado para codificar documentos de una manera que\
  \ es legible tanto para humanos como para m\xE1quinas."
title: Trabajando con XML
weight: 40
---

## Qué y Por Qué?
XML es un lenguaje de marcado para codificar documentos de una manera que es legible tanto para humanos como para máquinas. Es clave en servicios web, archivos de configuración e intercambio de datos porque transporta datos en un formato estructurado y jerárquico.

## Cómo hacerlo:
Clojure ofrece la biblioteca `clojure.data.xml` para el análisis y emisión de XML. Primero, vamos a analizar algo de XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [contenido "<root><foo>bar</foo><foo>baz</foo></root>"
      analizado (xml/parse-str contenido)] ; Analizar cadena XML
  (println analizado))
```
Salida:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Para emitir XML desde estructuras de Clojure:

```clojure
(def mi-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str mi-xml))
```
Salida:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Profundización
XML ha estado en el juego desde finales de los '90 como un subconjunto simplificado de SGML, destinado a datos web. Su uso explotó con tecnologías como SOAP y XHTML pero recibió un poco de competencia de JSON, que es preferido por su ligereza y simplicidad.

El enfoque de Clojure hacia XML lo mantiene funcional y centrado en los datos, siendo fiel al ethos del lenguaje. `clojure.data.xml` es solo una opción; tienes `clojure.xml` para necesidades básicas, y para la interoperabilidad con Java, puedes optar por pesos pesados como JAXB o DOM4J.

Ten en cuenta, el rendimiento y la sobrecarga de memoria al tratar con documentos XML muy grandes pueden ser considerables. Los analizadores de streaming como StAX pueden ayudar, pero necesitarás adentrarte en el territorio de Java para usarlos.

## Ver También
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API para el Procesamiento de XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
