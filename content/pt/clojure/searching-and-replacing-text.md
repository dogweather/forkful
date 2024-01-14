---
title:    "Clojure: Buscando e substituindo texto"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Se você já trabalhou com qualquer tipo de texto, provavelmente já precisou fazer uma tarefa repetitiva de substituição. Felizmente, com algumas ferramentas simples do Clojure, é possível automatizar esse processo e economizar muito tempo. Neste artigo, vamos explorar como utilizar as funções nativas do Clojure para pesquisar e substituir texto em suas strings.

## Como
A função básica para substituir texto no Clojure é a `replace` do namespace `clojure.string`. Vamos dar uma olhada em um exemplo simples de como utilizá-la:

```Clojure
(require '[clojure.string :as str])

(str/replace "Olá, mundo!" #".*n.*" "m8")
```

A saída deste código será `m8`, já que a regex que utilizamos para pesquisar é `".*n.*"`, que significa que qualquer caractere pode aparecer antes e depois da letra `n`.

Podemos também passar uma função como argumento para `replace` que será usada para alterar o texto encontrado pela nossa regex:

```Clojure
(str/replace "Esta é uma string qualquer" #"[aeiou]" (fn [x] (str/upper-case x)))
```

Neste exemplo, a saída será `EstA É UmA strIng quAlquEr`, já que utilizamos uma função anônima para transformar todas as vogais encontradas em vogais maiúsculas.

## Deep Dive
A função `replace` é uma ótima ferramenta para substituição de texto simples, mas se você precisar de mais controle sobre o processo, pode utilizar as funções `replace-first` e `replace-nth` do mesmo namespace.

A função `replace-first` substitui apenas a primeira ocorrência do texto encontrado pela regex, enquanto `replace-nth` substitui a n-ésima ocorrência. Além disso, você também pode utilizar regex e strings como argumentos para as funções `replace-first` e `replace-nth`, permitindo fazer substituições específicas em diferentes partes do texto.

## Veja também
[Documentação oficial do Clojure sobre strings](https://clojuredocs.org/clojure.string)

[Tutorial de strings do Clojure](https://www.tutorialspoint.com/clojure/clojure_strings.htm)

[Exemplos de uso avançado da função `replace` no Clojure](https://techdocs.io/blog/2016/11/03/sed-like-string-replace-in-clojure)