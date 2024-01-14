---
title:                "Clojure: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

A extração de substrings é uma tarefa comum em muitos projetos de programação. Se você está trabalhando com uma grande quantidade de dados ou precisa manipular cadeias de caracteres complexas, a extração de substrings pode ser uma ferramenta útil para simplificar seu código e torná-lo mais eficiente.

## Como Fazer

Para extrair substrings em Clojure, é necessário usar a função `subs`, que é nativa da linguagem. Esta função recebe duas entradas: a string original e um índice de início e fim, indicando a parte da string que você deseja extrair. Aqui está um exemplo de como usar esta função:

```Clojure
(def str "Olá Mundo!")
(subs str 0 3)
```

Neste caso, a substring extraída será "Olá". Você também pode usar índices negativos para começar a cortar a string a partir do final. Por exemplo:

```Clojure
(subs str -6 -1)
```

Isso irá extrair a substring "Mundo" da string original. Além disso, o índice final é opcional - se você não especificar um, a substring será extraída até o final da string.

## Mergulho Profundo

A função `subs` também oferece suporte a outras funcionalidades, como a inversão da ordem dos índices e a reversão da ordem dos caracteres extraídos. Você também pode usar o operador `len` para obter o comprimento da string original, o que facilita a manipulação de substrings em uma string de tamanho variável.

## Veja Também

- [Documentação oficial do subs](https://clojuredocs.org/clojure.core/subs)
- [Guia para manipulação de strings em Clojure](https://www.baeldung.com/clojure/strings)