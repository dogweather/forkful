---
date: 2024-01-20 17:42:11.964307-07:00
description: "Como Fazer: Historicamente, a manipula\xE7\xE3o de strings \xE9 um desafio\
  \ comum na programa\xE7\xE3o, a necessidade de deletar caracteres de strings surge\
  \ em diversos\u2026"
lastmod: '2024-04-05T21:53:46.503127-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a manipula\xE7\xE3o de strings \xE9 um desafio comum na\
  \ programa\xE7\xE3o, a necessidade de deletar caracteres de strings surge em diversos\
  \ contextos - desde processamento de textos a limpeza de dados para an\xE1lise."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como Fazer:
```Clojure
; Exemplo de como deletar todos os dígitos de uma string
(defn delete-digits [s]
  (clojure.string/replace s #"\d" ""))

; Uso e resultado
(println (delete-digits "C0d1g0 Cl0jur3 2023")) ; => "Cdg Cljur "
```

```Clojure
; Exemplo removendo todos os espaços
(defn delete-spaces [s]
  (clojure.string/replace s #" " ""))

; Uso e resultado
(println (delete-spaces "Olá Clojure! Bem-vindo ao mundo funcional.")) ; => "OláClojure!Bem-vindoaaomundofuncional."
```

## Mergulhando Fundo
Historicamente, a manipulação de strings é um desafio comum na programação, a necessidade de deletar caracteres de strings surge em diversos contextos - desde processamento de textos a limpeza de dados para análise. Clojure, herdando seu poder dos RegEx (expressões regulares) da JVM (Java Virtual Machine), oferece uma sintaxe concisa para tais tarefas utilizando `clojure.string/replace`.

Alternativas para deletar caracteres incluem usar funções de mais baixo nível, como `filter` e `remove`, que podem compor soluções mais customizadas, porém mais verbosas. Em termos de implementação, a eficiência é crucial quando se trata de operações de strings, especialmente em grandes volumes de dados - Clojure se beneficia da performance das implementações de strings da JVM, otimizadas ao longo dos anos.

## Veja Também
- [Clojure Documentation on clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure from the ground up: strings](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
- [StackOverflow: How to replace a character by a newline in Clojure](https://stackoverflow.com/questions/7299585/how-to-replace-a-character-by-a-newline-in-clojure)
