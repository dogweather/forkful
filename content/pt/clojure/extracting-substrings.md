---
title:                "Extraindo subcadeias"
html_title:           "Clojure: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

O que e Por Que?

Extrair substrings consiste em pegar um pedaço de uma string maior. Programadores fazem isso quando precisam manipular partes específicas de uma string, como um nome ou um número de telefone.

Como Fazer:

Para extrair substrings em Clojure, podemos usar a função subs, que pega uma string e retorna um pedaço específico dela. Por exemplo:

```Clojure
(subs "Olá Mundo" 0 3) ; retorna "Olá"
(subs "123-456-7890" 4 7) ; retorna "456"
```

Deep Dive:

Extrair substrings em Clojure é uma tarefa comum e muitas vezes necessária na programação. Existem alternativas para a função subs, como a função string-split, mas ela retorna uma lista em vez de uma string. A implementação da função subs é baseada no conceito de indexação de strings, em que cada caractere é numerado e pode ser acessado individualmente.

Veja Também:

Para mais informações sobre a função subs e outras funções relacionadas a strings em Clojure, consulte a documentação oficial: https://clojuredocs.org/clojure.string/subs.