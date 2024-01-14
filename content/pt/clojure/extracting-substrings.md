---
title:                "Clojure: Extraindo subcadeias"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings em Clojure?

Se você está trabalhando com strings em Clojure, é muito comum que em algum momento precise extrair uma parte específica dessa string. Isso pode ser útil para diversas finalidades, como por exemplo, validar um número de telefone, extrair o nome de um arquivo ou até mesmo realizar manipulações mais complexas nos dados. Neste artigo, vamos explorar como extrair substrings em Clojure de forma simples e eficiente.

## Como fazer?

Para extrair substrings em Clojure, utilizamos a função `subs`, que recebe como parâmetros a string e os índices do início e fim do trecho que queremos extrair. Vamos ver um exemplo prático:

```Clojure
(def texto "Hello World!")
(subs texto 2 6) ; retorna "llo "
;; O primeiro índice é inclusivo e o segundo é exclusivo, por isso o resultado não inclui o caracter na posição 6.
```

Além disso, é possível utilizar índices negativos que contam a partir do final da string. Por exemplo:

```Clojure
(def texto "Hello World!")
(subs texto -3 -1) ; retorna "ld"
;; O mesmo resultado seria obtido com (subs texto 8 10)
```

Também é possível utilizar o caracter `nil` para indicar o início ou fim da string. Se não for especificado um início, o valor padrão será 0, e se não for especificado um fim, a string completa será retornada. Por exemplo:

```Clojure
(def texto "Hello World!")
(subs texto nil 5) ; retorna "Hello"
(subs texto 6 nil) ; retorna "World!"
(subs texto nil) ; retorna "Hello World!"
```

## Uma olhada mais aprofundada

Agora que já sabemos como utilizar a função `subs` para extrair substrings, vamos dar uma olhada em alguns detalhes importantes a serem considerados. Primeiramente, é importante lembrar que as strings em Clojure são imutáveis, ou seja, não podemos modificar diretamente uma string existente. Portanto, ao extrair uma substring, na verdade estamos criando uma nova string com os caracteres desejados.

Além disso, é importante tomar cuidado com as diferenças entre índices e posições. Enquanto os índices começam em 0, as posições começam em 1. Por exemplo, a primeira letra da string "Hello World!" está na posição 1, mas na posição 0 não há nenhum caracter.

Por fim, é sempre bom lembrar que a função `subs` retorna uma nova string, portanto é necessário atribuir esse valor a uma variável ou utilizá-lo de alguma forma, caso contrário, a substring será criada mas não será salva em lugar algum.

## Veja também

Aqui estão alguns links úteis para você se aprofundar ainda mais no assunto:

- Documentação oficial sobre a função `subs`: https://clojuredocs.org/clojure.core/subs
- Outras formas de manipluar strings em Clojure: https://clojuredocs.org/clojure.string/replace
- Uma explicação mais detalhada sobre índices e posições em Clojure: https://clojurebridge.github.io/community-docs/docs/clojure/strings/