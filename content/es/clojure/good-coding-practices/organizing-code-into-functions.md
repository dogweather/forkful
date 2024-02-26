---
date: 2024-01-26 01:09:39.129860-07:00
description: "Partir el c\xF3digo en funciones se trata de empaquetar bloques de c\xF3\
  digo que cumplen tareas espec\xEDficas. Hacer esto vuelve tu c\xF3digo limpio, m\xE1\
  s f\xE1cil de\u2026"
lastmod: '2024-02-25T18:49:55.223697-07:00'
model: gpt-4-1106-preview
summary: "Partir el c\xF3digo en funciones se trata de empaquetar bloques de c\xF3\
  digo que cumplen tareas espec\xEDficas. Hacer esto vuelve tu c\xF3digo limpio, m\xE1\
  s f\xE1cil de\u2026"
title: "Organizando c\xF3digo en funciones"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Partir el código en funciones se trata de empaquetar bloques de código que cumplen tareas específicas. Hacer esto vuelve tu código limpio, más fácil de mantener, y un soplo de aire fresco para que otros desarrolladores lo lean.

## Cómo hacerlo:

Las funciones en Clojure se definen con `defn`, seguido por un nombre, parámetros y cuerpo. Aquí hay un ejemplo rápido.

```Clojure
(defn greet [name]
  (str "Hola, " name "!"))

(greet "Alex") ; => "Hola, Alex!"
```

Ahora supongamos que queremos calcular el área de un rectángulo. En vez de mezclarlo todo junto, lo separamos en dos funciones:

```Clojure
(defn area [largo ancho]
  (* largo ancho))

(defn print-area [largo ancho]
  (println "El área es:" (area largo ancho)))

(print-area 3 4) ; => El área es: 12
```

## Profundización

Hace tiempo, los programadores simplemente amontonaban toda su lógica en un solo bloque. Era feo. Luego llegó la programación estructurada, y las funciones se convirtieron en algo importante. En Clojure, cada función es de primera clase—puedes manejarlas como cualquier otro valor.

¿Alternativas? Algunas personas podrían jugar con métodos múltiples o funciones de orden superior, pero esos son solo condimentos en el guiso de funciones.

Todos los detalles en las funciones: son inmutables en Clojure, lo que hace menos probable los enredos de efectos secundarios. Apoyan mucho en la recursión en lugar de bucles típicos, lo cual se alinea bien con los paradigmas funcionales del lenguaje.

## Ver También

- Guía propia de Clojure: https://clojure.org/guides/learn/functions
- Fundamentos de la Programación Funcional: https://www.braveclojure.com/core-functions-in-depth/
- Charlas de Rich Hickey: https://changelog.com/posts/rich-hickeys-greatest-hits - para entender la filosofía de Clojure.
