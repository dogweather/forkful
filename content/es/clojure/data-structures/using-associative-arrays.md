---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:05.523071-07:00
description: "Los arreglos asociativos, o mapas hash, en Clojure te permiten almacenar\
  \ y recuperar datos con pares clave-valor. Son una opci\xF3n predilecta para manejar\u2026"
lastmod: 2024-02-19 22:05:17.240956
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, o mapas hash, en Clojure te permiten almacenar\
  \ y recuperar datos con pares clave-valor. Son una opci\xF3n predilecta para manejar\u2026"
title: Uso de matrices asociativas
---

{{< edit_this_page >}}

## Qué y Por Qué

Los arreglos asociativos, o mapas hash, en Clojure te permiten almacenar y recuperar datos con pares clave-valor. Son una opción predilecta para manejar datos estructurados, lo que hace que sea más rápido acceder a elementos específicos sin tener que iterar a través de una lista.

## Cómo:

En Clojure, crear y manipular arreglos asociativos (mapas hash) es sencillo. Vamos a profundizar con ejemplos.

Para crear un mapa hash:

```clojure
(def my-map {:name "Alex" :age 30})
```

Puedes recuperar un valor especificando su clave:

```clojure
(get my-map :name)
;; "Alex"
```
O, de manera más idiomática, puedes usar la clave como una función:

```clojure
(:name my-map)
;; "Alex"
```

Agregar o actualizar entradas es simple:

```clojure
(def updated-map (assoc my-map :location "Nueva York"))
;; {:name "Alex", :age 30, :location "Nueva York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Para remover claves, usa `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Para iterar sobre un mapa:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Y para acceso condicional, `find` devuelve un par clave-valor si la clave existe:

```clojure
(find my-map :age)
;; [:age 30]
```

## Análisis Profundo

Los arreglos asociativos en Clojure, comúnmente referidos como mapas hash, son increíblemente versátiles y eficientes para manejar datos basados en clave-valor. Son parte de la rica biblioteca de colecciones de Clojure, profundamente arraigados en la filosofía del lenguaje de inmutabilidad y programación funcional. A diferencia de los arreglos o listas que requieren una complejidad de tiempo O(n) para acceder a los elementos, los mapas hash proporcionan una complejidad de tiempo casi constante para el acceso, lo que los hace altamente eficientes para operaciones de búsqueda.

Uno podría argumentar que los vectores en Clojure podrían servir un propósito similar a través del acceso indexado, pero los mapas hash brillan cuando se trata de lidiar con datos no secuenciales y etiquetados, donde la clave proporciona un descriptor significativo en lugar de un índice arbitrario.

Único en Clojure (y su herencia Lisp), los arreglos asociativos son ciudadanos de primera clase, lo que significa que pueden ser manipulados directamente, pasados alrededor de funciones y más, sin necesitar una sintaxis especial o métodos de acceso. Esta decisión de diseño refuerza el énfasis de Clojure en la simplicidad y el poder.

Aunque los mapas hash son increíblemente útiles, vale mencionar que para conjuntos de datos muy grandes o escenarios donde las claves son altamente dinámicas (adición y remoción constante), estructuras de datos alternativas o bases de datos podrían ofrecer un mejor rendimiento y flexibilidad. Sin embargo, para la mayoría de los casos de uso típicos dentro del ámbito de las aplicaciones Clojure, los arreglos asociativos proporcionan un medio robusto y eficiente de gestión de datos.
