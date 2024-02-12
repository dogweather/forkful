---
title:                "Refatoração"
aliases:
- /pt/clojure/refactoring.md
date:                  2024-01-26T01:17:22.183084-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Refatoração é o processo de reestruturar o código de computador existente sem alterar seu comportamento externo, visando melhorar atributos não funcionais. Programadores refatoram para tornar seu código mais limpo, eficiente e fácil de manter, melhorando efetivamente a legibilidade e reduzindo a complexidade de seu software.

## Como Fazer:

Refatorar em Clojure—graças à sua sintaxe limpa e ao paradigma funcional—pode ser incrivelmente direto. Vamos abordar um cenário comum: iterar sobre coleções. Você pode começar com um loop `for`, assim:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Chamar `(old-way)` nos dará 55, a soma de 1 a 10. Mas, ei, podemos refatorar isso para ser mais Clojure-esque:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Esta função refatorada `(new-way)` usa macros de threading para passar o intervalo diretamente para `reduce`, cortando o excesso.

## Mergulho Profundo

A arte da refatoração tem suas raízes nos primeiros dias do desenvolvimento de software, mas realmente ganhou tração com o livro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" publicado em 1999. Em Clojure, a refatoração muitas vezes se apoia nos princípios da programação funcional, favorecendo funções puras e estruturas de dados imutáveis.

Alternativas à refatoração manual em Clojure poderiam incluir o uso de ferramentas como Cursive, um plugin popular do IntelliJ IDEA, que oferece refatorações automatizadas específicas para Clojure. Há também o clj-refactor, um pacote Emacs para Clojure, fornecendo um conjunto de funções de refatoração.

Um desafio peculiar à refatoração em Clojure é lidar com o estado e efeitos colaterais em um paradigma principalmente imutável e livre de efeitos colaterais. O uso cuidadoso de átomos, refs, agentes e transientes são fundamentais na manutenção de desempenho e correção durante as refatorações.

## Veja Também

- "Refactoring: Improving the Design of Existing Code" de Martin Fowler para os conceitos fundamentais.
- [Clojure Docs](https://clojuredocs.org/) para exemplos específicos de código Clojure idiomático.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) para automação de refatoração no Emacs.
- [Cursive](https://cursive-ide.com/) para usuários do IntelliJ buscando assistência de refatoração automatizada.
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Uma palestra pelo criador do Clojure que, embora não seja sobre refatoração per se, fornece insights sobre a filosofia Clojure que pode guiar decisões de refatoração eficazes.
