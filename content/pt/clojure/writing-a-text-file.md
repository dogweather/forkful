---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Gravar um arquivo de texto é o processo de salvar dados em forma de texto em um arquivo no seu sistema de arquivos. Programadores fazem isso para persistir informações, configurar sistemas, ou simplesmente para registrar logs de um programa.

## Como Fazer:

Em Clojure, você pode escrever em arquivos de texto usando funções como `spit` ou bibliotecas especializadas para manipulação de arquivos.

```clojure
;; Uso simples do `spit` para escrever em um arquivo
(spit "exemplo.txt" "Olá, Clojure!")

;; Adicionando linhas a um arquivo existente
(spit "exemplo.txt" "Mais uma linha." :append true)
```

Após executar esse código, você terá um arquivo chamado `exemplo.txt` com o conteúdo:

```
Olá, Clojure!
Mais uma linha.
```

## Aprofundando

Historicamente, a manipulação de arquivos em Lisps, como Clojure, tem suas raízes em abstrações fornecidas desde os primeiros dias da programação funcional. Comparado com outras linguagens que oferecem múltiplas maneiras de realizar essa tarefa, Clojure mantém a filosofia de simplicidade, ofertando poucas e poderosas abstrações.

Alternativas incluem bibliotecas como `clojure.java.io` para mais funcionalidades e controle detalhado sobre a leitura e escrita de arquivos.

Detalhes de implementação envolvem tratar corretamente de codificação de caracteres (tipicamente UTF-8) e gerenciamento de recursos, como fechar arquivos após o uso para evitar vazamentos de recursos.

## Veja Também

- Clojure Documentation: https://clojure.org/
- `clojure.java.io` API: https://clojuredocs.org/clojure.java.io
- Clojure Quick Reference: https://clojuredocs.org/quickref
