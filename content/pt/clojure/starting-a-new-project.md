---
title:    "Clojure: Iniciando um novo projeto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

Por que iniciar um novo projeto em Clojure?

Clojure é uma linguagem de programação funcional dinâmica e moderna que tem sido cada vez mais adotada por desenvolvedores em todo o mundo. Além disso, ela é construída na plataforma JVM, o que significa que pode ser usada junto com outras linguagens Java para criar aplicativos poderosos e escalonáveis.

## Como começar

Antes de começar um novo projeto em Clojure, é importante ter uma compreensão básica da sintaxe e dos conceitos da linguagem. Uma das melhores maneiras de fazer isso é seguindo tutoriais online ou lendo um livro sobre Clojure.

Uma vez que você tenha uma base sólida, é hora de começar a escrever seu código. Aqui está um exemplo simples de um programa que imprime "Hello World!" na tela:

```Clojure
(ns meu-projeto.core
  (:require [clojure.main :refer [main]]))

(defn -main []
  (println "Hello World!"))
```

Ao rodar esse código no terminal, você deve ver a seguinte saída:

```Hello World!```

Nosso programa está funcionando corretamente!

## Aprofundando

Além da sintaxe básica, é importante entender os principais conceitos por trás de Clojure. Aqui estão algumas coisas que você pode se aprofundar:

- Imutabilidade e estruturas de dados persistentes
- Funções de ordem superior e programação funcional
- Multithreading e concorrência
- Gerenciamento de dependências com leiningen ou boot

Natualmente, há muito mais para aprender sobre Clojure, mas esses conceitos básicos devem ajudar a dar uma visão geral do que é essa linguagem e como ela é diferente de outras.

## Veja também

Aqui estão algumas recomendações de recursos adicionais para aprender mais sobre Clojure:

- [Site oficial de Clojure](https://clojure.org/)
- [Documentação da linguagem Clojure](https://clojure.org/guides/learn/syntax)
- [Livro "Clojure for the Brave and True"](https://www.braveclojure.com/)
- [Comunidade ClojureBR no Slack](https://clojure-br.slack.com/)