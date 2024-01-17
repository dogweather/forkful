---
title:                "Escrevendo no erro padrão"
html_title:           "Clojure: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Escrever para a saída padrão de erro, também conhecida como "standard error", é uma prática comum entre os programadores. Isso permite que eles vejam mensagens de erro e outras informações relevantes durante a execução de um programa.

Os programadores utilizam a saída padrão de erro para identificar e corrigir erros em seus códigos. Isso permite uma depuração mais eficiente e rápida do programa em comparação com apenas usar a saída padrão.

## Como fazer:
```
Clojure (prn "Mensagem de erro")  ;; escreve uma mensagem de erro para a saída padrão de erro
```

Exemplo de saída:
```
Mensagem de erro
```

## Aprofundando:
A prática de escrever para a saída padrão de erro tem suas raízes nas primeiras linguagens de programação, como o C. Naquela época, os programadores viam a saída padrão de erro como uma forma simples e rápida de identificar problemas em seus códigos.

Uma alternativa ao uso da saída padrão de erro é o uso de um sistema de log mais avançado, que registra e armazena informações de erros em um arquivo. No entanto, para pequenos programas ou scripts, escrever para a saída padrão de erro ainda é uma prática útil e eficiente.

Em Clojure, tanto a função `pr` quanto a função `prn` podem ser usadas para escrever na saída padrão de erro. A diferença é que a `prn` adiciona uma linha em branco após a mensagem e a `pr` não.

## Veja também:
- [Documentação oficial de Clojure](https://clojure.org/reference/io)
- [Artigo sobre erros e saídas em Clojure](https://www.newbedev.com/clojure/what-is-the-difference-between-print-and-println-in-clojure/)