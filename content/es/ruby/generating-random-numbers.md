---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generando Números Aleatorios en Ruby

## ¿Qué & Por qué?
Generar números aleatorios consiste en crear valores que no siguen un patrón predecible. Los programadores lo hacen para una variedad de propósitos, como realizar pruebas aleatorias y crear comportamientos impredecibles en los juegos.

## ¿Cómo Hacerlo?
Vamos a conocer cómo generar números aleatorios en Ruby.

```Ruby
# Generar un número aleatorio
puts rand(0...10) 
```

La salida puede variar ya que produce un número aleatorio entre 0 y 9.

```Ruby
# Generar un número aleatorio flotante
puts rand
```
La salida será un número flotante entre 0 y 1.

## Buceo Profundo
Generar números aleatorios tiene una rica tradición en la computación y ha sido crucial en el enfoque matemático para los problemas. Para propósitos no criptográficos, utilizamos el método `rand` en Ruby, pero existen alternativas como el módulo `SecureRandom` para fines criptográficos.

La generación de números aleatorios en Ruby está basada en un algoritmo pseudoaleatorio. Esto significa que aunque los números parecen aleatorios, si conoces los detalles de implementación, puedes predecir los números. Si necesitas un verdadero número aleatorio para propósitos como seguridad o criptografía, tendrías que buscar una fuente de entropía externa.

## Ver También
1. [La Doc oficial de `rand` en Ruby](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
2. [SecureRandom](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html)