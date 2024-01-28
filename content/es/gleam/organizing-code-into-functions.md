---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:10:21.060089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Organizar el código en funciones significa desglosar el comportamiento de un programa en partes más pequeñas y reutilizables. Los programadores hacen esto para que el código sea más claro, más fácil de mantener y para evitar la repetición.

## Cómo hacerlo:
Aquí hay un ejemplo simple de cómo organizar el código en funciones en Gleam:

```gleam
fn sumar(x, y) {
  x + y
}

fn principal() {
  let suma = sumar(3, 4)
  suma
}

// Salida de muestra
// 7
```

En este fragmento, `sumar` es una función que toma dos valores y los suma. `principal` es donde llamamos a `sumar` y manejamos el resultado.

## Análisis Profundo
Históricamente, el concepto de funciones (o "subrutinas") revolucionó la programación, allanando el camino para la programación estructurada en la década de 1960 y más allá. Las funciones fomentan un enfoque modular, donde los problemas se dividen en subproblemas, se resuelven independientemente y se componen para resolver el problema mayor.

En Gleam, que es fuertemente tipado, las funciones también llevan información de tipo, asegurando que su uso sea consistente con su definición. Esto reduce errores y aclara las intenciones.

Las alternativas a las funciones incluyen la codificación en línea, donde la lógica se escribe repetidamente. Aunque a veces es más rápida para tareas pequeñas y únicas, la codificación en línea no escala bien para aplicaciones más grandes.

Los detalles de implementación a considerar al organizar en funciones pueden incluir la composición de funciones, donde las funciones se utilizan como bloques de construcción, y las funciones de orden superior, que toman otras funciones como argumentos o las devuelven, agregando flexibilidad a cómo se organiza y ejecuta el código.

## Ver También
Para más información sobre funciones en Gleam, puedes sumergirte en la documentación oficial en:
- [Funciones del lenguaje Gleam](https://gleam.run/book/tour/functions.html)

O explorar conceptos de programación más amplios:
- [Red de Desarrolladores de Mozilla sobre Funciones en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Functions)
- [Learn You Some Erlang for Great Good! - Sobre Módulos y Funciones](https://learnyousomeerlang.com/modules)
