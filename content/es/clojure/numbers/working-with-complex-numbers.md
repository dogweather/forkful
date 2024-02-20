---
date: 2024-01-26 04:38:32.520940-07:00
description: "Los n\xFAmeros complejos extienden los n\xFAmeros reales con una parte\
  \ adicional, la unidad imaginaria 'i'. Los programadores los utilizan en varios\
  \ dominios,\u2026"
lastmod: 2024-02-19 22:05:17.241963
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos extienden los n\xFAmeros reales con una parte adicional,\
  \ la unidad imaginaria 'i'. Los programadores los utilizan en varios dominios,\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos extienden los números reales con una parte adicional, la unidad imaginaria 'i'. Los programadores los utilizan en varios dominios, incluyendo el procesamiento de señales, la teoría electromagnética y los fractales, donde los cálculos que involucran la raíz cuadrada de un número negativo son rutinarios.

## Cómo hacerlo:
Clojure ofrece soporte incorporado para los números complejos a través de la clase utilitaria `clojure.lang.Numbers`. Utiliza `complex` para crear números complejos y realizar aritmética.

```clojure
;; Creando números complejos
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Adición
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Sustracción
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplicación
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; División
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Conjugado
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Profundización
Los números complejos fueron formalizados por matemáticos como Gauss y Euler en el siglo XVIII. Aunque inicialmente fueron recibidos con escepticismo, desde entonces se han vuelto cruciales en la ciencia y la ingeniería modernas. Clojure no tiene un tipo nativo de número complejo como algunos lenguajes (por ejemplo, Python), pero la interop con Java incluida puede manejar las operaciones necesarias a través de la clase `clojure.lang.Numbers`.

La `java.lang.Complex` de Java es una alternativa robusta, que ofrece más características y potenciales optimizaciones. La interoperabilidad de Clojure con su entorno permite trabajar fácilmente con las bibliotecas de Java.

Por debajo, la aritmética de los números complejos implica sumar y multiplicar las partes reales e imaginarias, con la regla clave de que `i^2 = -1`. La división de números complejos puede ser más complicada, típicamente requiriendo el conjugado para evitar la división por números complejos.

## Ver también
- Los ClojureDocs, para una referencia rápida: https://clojuredocs.org/
- La API de Java para `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- La página de Wikipedia sobre números complejos para los curiosos matemáticamente: https://en.wikipedia.org/wiki/Complex_number
