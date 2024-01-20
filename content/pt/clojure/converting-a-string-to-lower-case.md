---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma string em minúsculas é transformar todas as letras maiúsculas de uma string em letras minúsculas. Programadores fazem isso para padronizar os dados e facilitar as comparações entre as strings. 

## Como Fazer:

Aqui temos um exemplo de como converter uma string em minúsculas em Clojure:

```clojure
(-> "Olá, Pessoas Lindas! Como Vocês Estão?"
    (clojure.string/lower-case))
```

Ao executar o código acima, o resultado será:

```
"olá, pessoas lindas! como vocês estão?"
```

## Mergulho Profundo:

Clojure, uma linguagem moderna na plataforma JVM, tem uma abordagem particular para o problema corriqueiro de converter uma string em minúsculas. Em sua essência, Clojure usa a função `clojure.string/lower-case` que internamente chama o método `toLowerCase` do Java. Isso faz com que o comportamento desta operação em Clojure seja dependente da implementação da JVM e também do locale da instância JVM atual.

Uma alternativa seria usar uma biblioteca de manipulação de strings de terceiros, ou criar sua própria função, caso precisasse de um comportamento específico para sua aplicação.

## Veja Também:

1. [API de Strings em Clojure](https://clojure.github.io/clojure/clojure.string-api.html)
2. [Documentação do Clojure](https://clojure.org/api/api)
3. [Blogs de Programação em Clojure](http://planet.clojure.in/)
  
Lembre-se: a prática leva à perfeição. Mantenham a codificação!