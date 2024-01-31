---
title:                "Capitalizando uma string"
date:                  2024-01-19
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string significa transformar todas as letras iniciais das palavras em maiúsculas. Programadores fazem isso para padronizar dados, formatar textos de modo apresentável ou atender a regras gramaticais específicas.

## Como Fazer:
```Clojure
(defn capitalize [s]
  (clojure.string/capitalize s))

(capitalize "olá, mundo!") ; => "Olá, Mundo!"
(capitalize "clojure é demais!") ; => "Clojure É Demais!"
```

## Mergulho Profundo
Capitalizar strings não é um conceito novo; sua utilidade estende-se desde os primórdios da tipografia. Em Clojure, a função `clojure.string/capitalize` é prática, mas capitaliza apenas a primeira letra da string inteira. Para capitalizar todas as palavras, você terá que fazer um pouco mais: divida a string em palavras, capitalize cada uma e junte tudo de novo.

Outra abordagem é usar expressões regulares com `clojure.string/replace` para buscar o início de cada palavra e capitalizá-lo, ou seja, uma função personalizada que lida com casos mais complicados, como abreviações e nomes próprios.

A implementação também deve considerar caracteres especiais e acertos de desempenho para strings muito longas, mas, no dia a dia, a biblioteca `clojure.string` já resolve a maioria das necessidades.

```Clojure
(defn capitalize-words [s]
  (->> s
       (clojure.string/split #" ")
       (map clojure.string/capitalize)
       (clojure.string/join " ")))

(capitalize-words "bem-vindo ao mundo de clojure!") ; => "Bem-Vindo Ao Mundo De Clojure!"
```

## Veja Também:
- Documentação oficial do Clojure para `clojure.string`: [clojure.org](https://clojure.org/api/cheatsheet)
- Uma discussão sobre a manipulação de strings no StackOverflow: [StackOverflow Clojure Strings](https://stackoverflow.com/questions/tagged/clojure+string)
