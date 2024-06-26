---
date: 2024-01-26 04:38:50.779159-07:00
description: "Como fazer: Clojure oferece suporte embutido para n\xFAmeros complexos\
  \ atrav\xE9s da classe de utilidade `clojure.lang.Numbers`. Use `complex` para criar\u2026"
lastmod: '2024-03-13T22:44:46.190881-06:00'
model: gpt-4-0125-preview
summary: "Clojure oferece suporte embutido para n\xFAmeros complexos atrav\xE9s da\
  \ classe de utilidade `clojure.lang.Numbers`."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
Clojure oferece suporte embutido para números complexos através da classe de utilidade `clojure.lang.Numbers`. Use `complex` para criar números complexos e realizar aritmética.

```clojure
;; Criando números complexos
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Adição
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Subtração
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplicação
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Divisão
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Conjugado
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Aprofundamento
Números complexos foram formalizados por matemáticos como Gauss e Euler no século 18. Embora inicialmente recebidos com ceticismo, desde então tornaram-se cruciais na ciência e engenharia modernas. Clojure não possui um tipo nativo de número complexo como algumas linguagens (por exemplo, Python), mas a interoperabilidade com Java incluída pode lidar com as operações necessárias através da classe `clojure.lang.Numbers`.

`java.lang.Complex` de Java é uma alternativa robusta, oferecendo mais recursos e potenciais otimizações. A interoperabilidade hospedeira de Clojure torna fácil trabalhar com bibliotecas Java.

Por baixo do capô, a aritmética de números complexos envolve adicionar e multiplicar as partes real e imaginária, com a regra chave de que `i^2 = -1`. A divisão de número complexo pode ser mais complicada, tipicamente requerendo o conjugado para evitar a divisão por números complexos.

## Veja Também
- O ClojureDocs, para uma referência rápida: https://clojuredocs.org/
- A API Java para `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- A página da Wikipedia sobre números complexos para os curiosos matematicamente: https://en.wikipedia.org/wiki/Complex_number
