---
title:                "Extraindo Substrings."
html_title:           "Elm: Extraindo Substrings."
simple_title:         "Extraindo Substrings."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Extrair substrings é o ato de selecionar um pedaço de uma string maior. Programadores frequentemente fazem isso para manipular e analisar dados específicos dentro de uma string, especialmente quando se trata de trabalhar com dados de entrada do usuário.

## Como fazer:
```
Elm
import String

string = "Olá, mundo!"
primeiros5 = String.left 5 string
ultimos5 = String.right 5 string

-- Saída: "Olá," e "undo!"
```
Neste exemplo, usamos o módulo String do Elm para extrair os primeiros e últimos 5 caracteres da string "Olá, mundo!".

## Detalhando mais:
Extrair substrings é um recurso muito útil em diversas situações. Além de ser usado para manipular dados de entrada, também pode ser útil para formatar strings antes de enviá-las para outros sistemas ou para exibir informações específicas para o usuário.

Existem alternativas para realizar essa tarefa, como o uso de funções que percorrem a string e retornam o trecho desejado. No entanto, o uso do módulo String do Elm facilita e agiliza o processo.

No Elm, as strings são imutáveis, o que significa que não podem ser alteradas após serem criadas. Por isso, a extração de substrings é feita através de funções que criam uma nova string contendo o trecho selecionado da original.

## Veja também:
- [Documentação oficial do módulo String do Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Exemplos de códigos com o uso do módulo String](https://github.com/elm/core/tree/1.0.5/packages/core/src/String.elm)