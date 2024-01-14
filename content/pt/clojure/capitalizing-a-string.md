---
title:    "Clojure: Maiúscula de uma String"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de mudar a capitalização de uma string em seus projetos em Clojure? Talvez você queira imprimir uma saída com letras maiúsculas ou talvez precise fazer uma comparação de strings sem se preocupar com a capitalização. Qualquer que seja o motivo, aprender a capitalizar uma string pode ser útil em vários cenários.

## Como Fazer

Para capitalizar uma string em Clojure, podemos utilizar a função `clojure.string/capitalize`. Vamos dar uma olhada em alguns exemplos:

```
(ns capitalizar-string.core
  (:require [clojure.string :as str]))

(str/capitalize "olá mundo") ;; Saída: "Olá mundo"
(str/capitalize "EU GOSTO DE PROGRAMAR") ;; Saída: "Eu gosto de programar"
(str/capitalize "oNIBUs") ;; Saída: "Onibus"
```

Note que a função `capitalize` retorna uma nova string com a primeira letra em maiúscula e as demais letras em minúsculo. Agora, se você quiser capitalizar todas as palavras de uma string, podemos utilizar a função `clojure.string/capitalize-words` da seguinte forma:

```
(str/capitalize-words "aprendendo A PROGRAMAR em clojure") ;; Saída: "Aprendendo a programar em clojure"
```

## Mergulho Profundo

Ao capitalizar uma string em Clojure, é importante levar em consideração que a função `capitalize` e `capitalize-words` consideram como palavra apenas a parte que não é separada por espaços, ou seja, não é possível capitalizar todas as letras de uma string que contenha espaços. Além disso, essas funções também respeitam as regras de capitalização dos idiomas, como por exemplo, a regra de capitalização de nomes em inglês.

## Veja Também

- [Documentação oficial da função `capitalize`](https://clojuredocs.org/clojure.string/capitalize)
- [Documentação oficial da função `capitalize-words`](https://clojuredocs.org/clojure.string/capitalize-words)
- [Tutorial de Clojure para iniciantes](http://clojure-doc.org/articles/tutorials/introduction.html)