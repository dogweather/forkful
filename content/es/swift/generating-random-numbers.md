---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generando Números Aleatorios en Swift

## ¿Qué & Por qué?

La generación de números aleatorios es el proceso de producir números que no poseen ningún patrón predecible. Los programadores lo hacen para crear comportamientos impredecibles, usualmente en juegos, pruebas y simulaciones.

## Cómo hacerlo:

En Swift, puedes generar un número aleatorio simplemente usando la función `random(in:)` integrada.

```Swift
let aleatorio = Int.random(in: 1..<10)
print(aleatorio)
```
Este código genera un número entero aleatorio entre 1 y 9.

## Profundizando:

La generación de números aleatorios tiene una larga historia en informática. Anteriormente, era un proceso complicado que requería un generador de números pseudoaleatorios (PRNG) o un generador de números aleatorios verdaderos (TRNG). Hoy en día, la mayoría de los lenguajes de programación incluyen una forma sencilla de generar números aleatorios, como Swift con su función `random(in:)`.

Algunas alternativas para generar números aleatorios en Swift son usar la semilla de la hora actual o incluso utilizar una biblioteca externa para un mayor grado de aleatoriedad.

La implementación de `random(in:)` utiliza un generador de números aleatorios seguro. Esto significa que es más resistente a la predicción, haciéndolo adecuado para usos en los que la seguridad es importante.

## Ver También:

- Página de Wikipedia sobre la generación de números aleatorios: [https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios](https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios)