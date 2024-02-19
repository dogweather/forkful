---
aliases:
- /pt/clojure/concatenating-strings/
date: 2024-01-20 17:34:20.437638-07:00
description: "Concatenar strings significa juntar duas ou mais sequ\xEAncias de caracteres\
  \ para formar uma nova. Programadores fazem isso para construir mensagens,\u2026"
lastmod: 2024-02-18 23:08:57.794767
model: gpt-4-1106-preview
summary: "Concatenar strings significa juntar duas ou mais sequ\xEAncias de caracteres\
  \ para formar uma nova. Programadores fazem isso para construir mensagens,\u2026"
title: Concatenando strings
---

{{< edit_this_page >}}

## O que é & Por quê?
Concatenar strings significa juntar duas ou mais sequências de caracteres para formar uma nova. Programadores fazem isso para construir mensagens, combinar dados e manipular texto de maneira dinâmica.

## Como fazer:
Concatenar strings no Clojure é direto, graças à função `str`. Veja só:

```Clojure
;; Concatenando duas strings simples
(str "Olá, " "mundo!")
;; => "Olá, mundo!"

;; Concatenando múltiplas strings
(str "Clojure " "é " "elegante!")
;; => "Clojure é elegante!"

;; Juntando strings com números (e qualquer outro tipo)
(str "A resposta é: " 42)
;; => "A resposta é: 42"
```

Se você tem uma coleção de strings, use a função `join` da biblioteca `clojure.string`:

```Clojure
(require '[clojure.string :as str])

;; Usando join para concatenar com um separador
(str/join ", " ["Clojure" "Scala" "Haskell"])
;; => "Clojure, Scala, Haskell"
```

## Aprofundando
Concatenar strings é algo tão básico quanto pode ser na computação; está por aí desde os primórdios das linguagens de programação. Em Clojure, essa operação é super eficiente, porque Clojure é construída em cima da JVM (Java Virtual Machine), que é otimizada para operações com strings.

Alternativamente, pode-se usar a função `format` quando você precisa de mais controle sobre a formatação:

```Clojure
(format "O nome é %s e a idade é %d" "João" 30)
;; => "O nome é João e a idade é 30"
```

Outra coisa a saber é que, em Clojure, strings são imutáveis. Concatenar strings na verdade cria uma nova string a partir das existentes, sem modificar as originais.

## Veja também
- Documentação oficial do Clojure sobre strings: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
