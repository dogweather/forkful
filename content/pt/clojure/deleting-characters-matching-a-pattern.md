---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:11.964307-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Deletar caracteres que seguem um padrão é como tirar ervas daninhas do seu jardim de strings; às vezes, você só precisa de tulipas, não de dente-de-leão. Programadores fazem isso para limpar dados, remover informação desnecessária ou preparar strings para um formato específico.

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
