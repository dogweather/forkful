---
date: 2024-01-20 17:54:09.638781-07:00
description: "Ler um arquivo de texto \xE9 simplesmente acessar e interpretar o conte\xFA\
  do armazenado em um arquivo em seu disco. Programadores fazem isso para manipular\u2026"
lastmod: '2024-03-13T22:44:46.213835-06:00'
model: gpt-4-1106-preview
summary: "Ler um arquivo de texto \xE9 simplesmente acessar e interpretar o conte\xFA\
  do armazenado em um arquivo em seu disco. Programadores fazem isso para manipular\u2026"
title: Lendo um arquivo de texto
weight: 22
---

## O Que & Por Quê?

Ler um arquivo de texto é simplesmente acessar e interpretar o conteúdo armazenado em um arquivo em seu disco. Programadores fazem isso para manipular dados, configurar programas ou ler informações necessárias durante a execução de um software.

## Como Fazer:

```Clojure
;; Abrindo e lendo um arquivo linha por linha
(with-open [reader (clojure.java.io/reader "caminho/do/arquivo.txt")]
  (doseq [linha (line-seq reader)]
    (println linha)))
```

```Clojure
;; Lendo todo o conteúdo de uma só vez
(slurp "caminho/do/arquivo.txt")
```

```Clojure
;; Exemplo de saída de 'slurp'
"Isso é o conteúdo do arquivo texto, lido de uma vez só!"
```

## Mergulho Profundo:

Ler arquivos de texto é uma operação que data do início dos computadores: era uma forma básica de armazenar e recuperar informações. Em Clojure, você tem várias maneiras de ler arquivos. A função `slurp` é legal para arquivos pequenos, por ser rápida e direta. Mas, para arquivos maiores, ler linha por linha com `line-seq` é mais eficiente, pois consome menos memória. A Clojure, sendo uma linguagem funcional da JVM, usa as bibliotecas Java para input/output (I/O), então você tem acesso a todas as ferramentas do Java para ler arquivos, o que é uma enorme vantagem.

## Veja Também:

- Documentação oficial do Clojure sobre I/O: [clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Tutorial Clojure sobre manipulação de strings (útil após ler arquivos): [clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
