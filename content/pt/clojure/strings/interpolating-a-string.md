---
date: 2024-01-20 17:50:29.443077-07:00
description: "Interpola\xE7\xE3o de strings permite injetar vari\xE1veis diretamente\
  \ em um peda\xE7o de texto. Programadores fazem isso para construir strings de forma\
  \ din\xE2mica e\u2026"
lastmod: '2024-03-13T22:44:46.183483-06:00'
model: gpt-4-1106-preview
summary: "Interpola\xE7\xE3o de strings permite injetar vari\xE1veis diretamente em\
  \ um peda\xE7o de texto. Programadores fazem isso para construir strings de forma\
  \ din\xE2mica e\u2026"
title: Interpolando uma string
---

{{< edit_this_page >}}

## O que é e Por quê?
Interpolação de strings permite injetar variáveis diretamente em um pedaço de texto. Programadores fazem isso para construir strings de forma dinâmica e legível, economizando tempo e tornando o código mais limpo.

## Como fazer:
```Clojure
;; Usando str para concatenar strings e variáveis
(def nome "Mundo")
(str "Olá, " nome "!")

;; Saida: "Olá, Mundo!"

;; Usando um template literal com `format`
(format "Olá, %s!" nome)

;; Saida: "Olá, Mundo!"

;; Usando clojure.pprint/cl-format para mais complexidade
(require '[clojure.pprint :as pprint])
(pprint/cl-format nil "Olá, ~A!" nome)

;; Saida: "Olá, Mundo!"
```

## Mergulho Profundo
Interpolação de strings não é uma funcionalidade inerente na linguagem Clojure, ao contrário de linguagens como Ruby ou JavaScript. Em Clojure, geralmente utilizamos funções como `str` e `format`. Antes do Clojure, as linguagens Lisp já utilizavam funções de formato para interpolar strings, o que influenciou a abordagem do Clojure. 

Alternativamente, algumas bibliotecas de terceiros permitem interpolação com sintaxe mais sucinta. Por exemplo, a `clojure.string/interpolate` da biblioteca `selmer` permite interpolar com uma notação mais próxima de outras linguagens.

Quanto à implementação, `str` concatena valores convertendo-os para strings, enquanto `format` e `pprint/cl-format` utilizam placeholders específicos (`%s` para string no `format`, `~A` para argumentos automáticos no `pprint/cl-format`) para substituir valores na string resultante.

## Veja também
- [ClojureDocs para a função str](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs para a função format](https://clojuredocs.org/clojure.core/format)
- [Repositório GitHub da biblioteca Selmer](https://github.com/yogthos/Selmer)
