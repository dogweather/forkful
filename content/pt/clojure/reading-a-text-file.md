---
title:                "Lendo um arquivo de texto"
html_title:           "Clojure: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Porque?
Ler um arquivo de texto é uma tarefa comum na programação que envolve a leitura de dados armazenados em um arquivo de texto. Os programadores fazem isso para acessar e manipular informações relevantes contidas no arquivo, tornando possível a criação de programas mais dinâmicos e interativos.

## Como Fazer:
```Clojure
; Coletando informações de um arquivo de texto
(with-open [rdr (clojure.java.io/reader "arquivo.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

; Saída de exemplo:
; Este é um texto de exemplo.
; Ele será lido pelo programa e impresso na tela.
```
Para ler um arquivo de texto em Clojure, podemos usar a função `clojure.java.io/reader` e o laço `doseq` para percorrer cada linha do arquivo. Dentro do laço, usamos a função `println` para imprimir cada linha na tela.

## Profundidade:
Ler arquivos de texto é uma tarefa que se tornou mais importante com o avanço da tecnologia e do processamento de dados. Antes, a leitura de dados era feita apenas em discos rígidos, mas com o aumento no uso de dispositivos móveis e armazenamento em nuvem, a leitura de arquivos de texto em tempo real se tornou uma necessidade. Existem outras maneiras de ler arquivos de texto em Clojure, como o uso da biblioteca `clojure.tools.reader`, que fornece funções mais avançadas para a manipulação de arquivos.

## Veja Também:
- [Documentação do Clojure para a função `clojure.java.io/reader`](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/reader)
- [Documentação do Clojure para a biblioteca `clojure.tools.reader`](https://github.com/clojure/tools.reader)