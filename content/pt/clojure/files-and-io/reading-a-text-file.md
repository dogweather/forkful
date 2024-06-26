---
date: 2024-01-20 17:54:09.638781-07:00
description: "Como Fazer: Ler arquivos de texto \xE9 uma opera\xE7\xE3o que data do\
  \ in\xEDcio dos computadores: era uma forma b\xE1sica de armazenar e recuperar informa\xE7\
  \xF5es. Em\u2026"
lastmod: '2024-04-05T21:53:46.537842-06:00'
model: gpt-4-1106-preview
summary: "Ler arquivos de texto \xE9 uma opera\xE7\xE3o que data do in\xEDcio dos\
  \ computadores."
title: Lendo um arquivo de texto
weight: 22
---

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
