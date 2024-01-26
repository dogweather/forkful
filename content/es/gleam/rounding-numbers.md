---
title:                "Redondeo de números"
date:                  2024-01-26T03:45:39.040436-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Redondear números consiste en ajustar un valor al lugar especificado más cercano, como de 2.56 a 3 si estamos redondeando a números enteros. Los programadores hacen esto por simplicidad o para cumplir ciertas especificaciones numéricas, usualmente para evitar matices causados por errores de precisión en punto flotante o para hacer la salida amigable para el usuario.

## Cómo hacerlo:
En Gleam, el redondeo no está en la biblioteca estándar hasta la última vez que revisé, pero así es como normalmente redondearías un float al entero más cercano usando directamente funciones de Erlang:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Salida: 3
}
```

Salida:
```
3
```

¿Tienes en mente una precisión diferente? Digamos, ¿redondear a dos decimales? Necesitamos un poco de matemáticas:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Salida: 2.57
}
```

Salida:
```
2.57
```

## Análisis Profundo
Históricamente, redondear números ha sido crucial, especialmente en cálculos financieros y científicos donde la precisión y los estándares importan mucho. Sin el redondeo, acabarías con decimales largos y desagradables por todas partes, haciendo que los cálculos sean imprácticos y propensos a errores.

En el mundo de la programación, diferentes lenguajes ofrecen diferentes enfoques, desde funciones incorporadas hasta bibliotecas matemáticas completas. El redondeo podría involucrar diferentes reglas, por ejemplo, "redondear hacia arriba" (el método usual) o "redondear a par más cercano" (a menudo usado en cálculos financieros para evitar sesgos).

Gleam, siendo un lenguaje joven con raíces en Erlang, depende del robusto conjunto de funciones numéricas de Erlang. A medida que el lenguaje crece, podríamos ver funciones nativas introducidas, reduciendo la necesidad de llamar rutinas externas.

## Ver También
- Módulo :math de Erlang para más cálculos numéricos: https://erlang.org/doc/man/math.html
- Para antecedentes sobre por qué el redondeo puede ser complicado, el Estándar de Punto Flotante IEEE: https://ieeexplore.ieee.org/document/8766229
- Interesado en las matemáticas detrás de esto? Consulta "Lo que todo científico de la computación debería saber sobre aritmética de punto flotante": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html