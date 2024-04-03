---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:23.298680-07:00
description: "C\xF3mo hacerlo: Haskell no tiene arrays asociativos de manera inmediata\
  \ de la misma forma que algunos otros lenguajes, pero ofrece una biblioteca est\xE1\
  ndar\u2026"
lastmod: '2024-03-13T22:44:59.112284-06:00'
model: gpt-4-0125-preview
summary: "Haskell no tiene arrays asociativos de manera inmediata de la misma forma\
  \ que algunos otros lenguajes, pero ofrece una biblioteca est\xE1ndar poderosa llamada\
  \ `Data.Map` para trabajar con pares clave-valor."
title: Uso de matrices asociativas
weight: 15
---

## Cómo hacerlo:
Haskell no tiene arrays asociativos de manera inmediata de la misma forma que algunos otros lenguajes, pero ofrece una biblioteca estándar poderosa llamada `Data.Map` para trabajar con pares clave-valor. ¡Remanguémonos y veamos cómo usarlos!

Primero, asegúrate de importarla:
```Haskell
import qualified Data.Map as Map
```

Crear un mapa es sencillo. Creemos uno con algunos lenguajes de programación y sus paradigmas:
```Haskell
let languages = Map.fromList [("Haskell", "Funcional"), ("Python", "Imperativo"), ("Prolog", "Lógico")]
```

Ahora, ¿qué tal si obtenemos el paradigma de Haskell?
```Haskell
Map.lookup "Haskell" languages
-- salida: Just "Funcional"
```

Agregar un nuevo lenguaje es fácil:
```Haskell
let languagesUpdated = Map.insert "Rust" "Sistemas" languages
```

¿Y si queremos listar todos los lenguajes? Usa `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- salida: ["Haskell","Python","Prolog","Rust"]
```

Para listar los paradigmas, usa `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- salida: ["Funcional","Imperativo","Lógico","Sistemas"]
```

Estas operaciones básicas deberían cubrir la mayoría de los usos, ¡pero hay mucho más por explorar en `Data.Map`!

## Inmersión Profunda
El módulo `Data.Map` en la biblioteca estándar de Haskell está construido sobre árboles binarios equilibrados, específicamente árboles AVL. Esta elección asegura que la mayoría de las operaciones en el mapa, como la inserción, eliminación y búsqueda, puedan realizarse en un tiempo O(log n), donde n es el número de elementos en el mapa. Es una elección eficiente para muchos casos de uso, aunque no la más rápida absoluta para todos los escenarios.

Hay una matiz histórica también: antes de que `Data.Map` se convirtiera en la opción preferida, los programadores de Haskell a menudo usaban listas de pares para simular arrays asociativos. Sin embargo, las operaciones en tales estructuras son O(n) para la búsqueda, lo que hace a `Data.Map` una mejora significativa en términos de rendimiento.

Ahora, a pesar de la eficiencia y utilidad de `Data.Map`, no siempre es la mejor herramienta para cada trabajo. Para tareas altamente sensibles al rendimiento, donde incluso tiempos de búsqueda O(log n) son demasiado lentos, o donde las claves son siempre valores enteros, los arrays o tablas hash (vía `Data.HashMap`) podrían ofrecer un mejor rendimiento con tiempos de acceso O(1).

El ecosistema de Haskell permite una variedad de estructuras de datos para satisfacer diferentes necesidades, y `Data.Map` es una excelente elección de propósito general para arrays asociativos, equilibrando facilidad de uso, flexibilidad y rendimiento.
