---
aliases:
- /pt/clojure/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:43.372369-07:00
description: "Arrays associativos, ou mapas hash, em Clojure permitem que voc\xEA\
  \ armazene e recupere dados com pares de chave-valor. Eles s\xE3o uma escolha principal\
  \ para\u2026"
lastmod: 2024-02-18 23:08:57.795697
model: gpt-4-0125-preview
summary: "Arrays associativos, ou mapas hash, em Clojure permitem que voc\xEA armazene\
  \ e recupere dados com pares de chave-valor. Eles s\xE3o uma escolha principal para\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O quê & Por quê?

Arrays associativos, ou mapas hash, em Clojure permitem que você armazene e recupere dados com pares de chave-valor. Eles são uma escolha principal para gerenciar dados estruturados, facilitando o acesso a elementos específicos sem ter que iterar por uma lista.

## Como fazer:

Em Clojure, criar e manipular arrays associativos (mapas hash) é simples. Vamos mergulhar com exemplos.

Para criar um mapa hash:

```clojure
(def my-map {:name "Alex" :age 30})
```

Você pode recuperar um valor especificando sua chave:

```clojure
(get my-map :name)
;; "Alex"
```
Ou, de uma forma mais idiomática, você pode usar a chave como uma função:

```clojure
(:name my-map)
;; "Alex"
```

Adicionar ou atualizar entradas é simples:

```clojure
(def updated-map (assoc my-map :location "Nova York"))
;; {:name "Alex", :age 30, :location "Nova York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Para remover chaves, use `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Para iterar sobre um mapa:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

E para acesso condicional, `find` retorna um par chave-valor se a chave existir:

```clojure
(find my-map :age)
;; [:age 30]
```

## Aprofundando

Arrays associativos em Clojure, também comumente referidos como mapas hash, são incrivelmente versáteis e eficientes para gerenciar dados baseados em chave-valor. Eles fazem parte da rica biblioteca de coleções de Clojure, profundamente enraizada na filosofia de linguagem da imutabilidade e programação funcional. Ao contrário de arrays ou listas que requerem complexidade de tempo O(n) para acessar elementos, mapas hash fornecem uma complexidade de tempo quase constante para acesso, tornando-os altamente eficientes para operações de busca.

Alguém pode argumentar que vetores em Clojure poderiam servir a um propósito similar através do acesso indexado, mas os mapas hash brilham quando se trata de lidar com dados não sequenciais e rotulados, onde a chave provê um descritor significativo ao invés de um índice arbitrário.

Único ao Clojure (e sua herança Lisp), arrays associativos são cidadãos de primeira classe, significando que podem ser diretamente manipulados, passados por funções e mais, sem precisar de sintaxe especial ou métodos de acesso. Essa decisão de design reforça a ênfase do Clojure na simplicidade e poder.

Embora mapas hash sejam incrivelmente úteis, vale mencionar que para conjuntos de dados muito grandes ou cenários onde as chaves são altamente dinâmicas (adição e remoção constantes), estruturas de dados alternativas ou bancos de dados podem oferecer melhor desempenho e flexibilidade. No entanto, para a maioria dos casos de uso típicos dentro do domínio de aplicações Clojure, arrays associativos fornecem um meio robusto e eficiente de gerenciamento de dados.
