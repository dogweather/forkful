---
title:                "Clojure: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Clojure?

Ler e manipular arquivos de texto é uma tarefa comum em muitos projetos de software. Em Clojure, essa tarefa é simples e eficiente, permitindo que desenvolvedores possam facilmente trabalhar com dados armazenados em arquivos. 

## Como fazer?

```Clojure
;; Primeiro, precisamos importar a biblioteca "java.io"
(import java.io) 

;; Em seguida, podemos utilizar a função "slurp" para ler o conteúdo de um arquivo.
(def texto (slurp "arquivo.txt")) 

;; Para imprimir esse texto na tela, podemos usar a função "println"
(println texto) 

;; Se quisermos armazenar as linhas do arquivo em uma lista, podemos usar a função "line-seq" e a função "map"
(def linhas (map read-line (line-seq "arquivo.txt"))) 

;; Podemos utilizar a função "count" para saber quantas linhas foram lidas
(println (count linhas)) 

;; Para escrever em um arquivo, podemos utilizar a função "spit"
(def novo-texto "Novo texto para ser escrito no arquivo")
(spit "arquivo.txt" novo-texto)
```

## Aprofundando-se

A biblioteca "java.io" oferece várias funções úteis para trabalhar com arquivos de texto em Clojure. Além disso, com a ajuda da função "map", é possível aplicar transformações nos dados lidos do arquivo de forma eficiente. Também é importante lembrar de fechar o arquivo após a leitura ou escrita, utilizando a função "close".

## Veja também

- Documentação do Clojure: https://clojure.org/
- Biblioteca "java.io": https://clojure.github.io/clojure/clojure.java.io-api.html
- Funções para manipular coleções em Clojure: https://clojure.org/reference/sequences