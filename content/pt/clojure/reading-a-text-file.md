---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Ler um arquivo de texto é o processo de acessar e interpretar informações contidas no arquivo texto através da programação. Programadores fazem isso para manipular dados, analisar informações ou simplesmente para extrair conteúdo relevante.

## Como fazer:

Aqui está um exemplo simples de como ler um arquivo de texto na Clojure usando a função `slurp`.

```Clojure
(def texto (slurp "/caminho/para/seu/arquivo.txt"))  
(println texto)  
```

Ao executar esse código, Clojure lê o arquivo no caminho especificado e grava o conteúdo no `def texto`. A função `println` imprimirá o conteúdo no terminal.

## Mergulho profundo:

Clojure, uma linguagem de programação Lisp moderna, utiliza a JVM (Java Virtual Machine), o que proporciona a vantagem de utilizar a imensa biblioteca de classes Java. Dito isso, ler e escrever arquivos em Clojure não é um assunto novo. A função `slurp` que usamos acima é, de fato, um encapsulamento da funcionalidade fornecida pela Java.

Alternativas para `slurp` incluem `line-seq` que lê um arquivo linha por linha, útil quando o arquivo de texto é grande e a memória é uma preocupação.

```Clojure
(with-open [rdr (reader "/caminho/para/seu/arquivo.txt")]
  (doseq [linha (line-seq rdr)]
    (println linha)))
```

Essa função permite que cada linha seja processada individualmente, economizando memoria.

## Veja também:

A linguagem Clojure oferece diversas funções e bibliotecas para trabalhar com arquivos e streams. Aqui estão algumas leituras suplementares e fontes úteis:

- Documentação oficial da Clojure: [https://clojure.org/](https://clojure.org/)
- Biblioteca do Clojure para manipulação de arquivos: [https://clojuredocs.org/clojure.java.io](https://clojuredocs.org/clojure.java.io)
- Guia para 'slurp' sobre ClojureDocs: [https://clojuredocs.org/clojure.core/slurp](https://clojuredocs.org/clojure.core/slurp)