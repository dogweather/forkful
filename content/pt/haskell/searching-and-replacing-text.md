---
title:                "Busca e substituição de texto"
html_title:           "Haskell: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores fazem isso?

Substituir e buscar texto é uma tarefa comum para programadores que trabalham com linguagens de programação. Isso envolve encontrar uma determinada sequência de caracteres em um texto e substituí-la por outra. Os programadores geralmente fazem isso para corrigir erros, atualizar informações ou automatizar tarefas repetitivas.

## Como fazer:

Substituir e buscar texto em Haskell é fácil e simples. Vamos ver alguns exemplos usando a função `replace` do pacote `Text`.

```
import Data.Text

let texto = "Olá mundo!"

replace "mundo" "Haskell" texto 
-- saída: "Olá Haskell!"
```

Podemos também usar a função `replace` em uma lista de strings.

```
let lista = ["Olá", "mundo!", "Como", "você", "está?"]

map (replace "mundo" "Haskell") lista 
-- saída: ["Olá", "Haskell!", "Como", "você", "está?"]
```

## Mergulho Profundo:

A substituição e busca de texto é uma tarefa antiga, com suas primeiras formas sendo feitas manualmente através de editores de texto. Com o avanço da tecnologia, surgiram diversas ferramentas e linguagens de programação voltadas para a manipulação de texto. Alternativas em Haskell incluem o uso da biblioteca `regex` para usar expressões regulares em busca e substituição, ou a função `substitute` do pacote `regex-base` para substituir texto especificando a posição exata da string a ser substituída.

## Veja também:

- [Documentação da função `replace`](https://hackage.haskell.org/package/text/docs/Data-Text.html#v:replace)
- [Pacote `regex` para uso de expressões regulares em Haskell](https://hackage.haskell.org/package/regex)
- [Pacote `regex-base` para substituição de texto em uma posição específica](https://hackage.haskell.org/package/regex-base)