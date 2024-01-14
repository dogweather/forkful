---
title:    "Elm: Excluindo caracteres que correspondem a um padrão"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que?

Às vezes, em programação, nos deparamos com a necessidade de deletar caracteres que correspondem a um determinado padrão. Isso pode ser feito por diversos motivos, como por exemplo, por questões de segurança ou para a limpeza de dados.

## Como fazer?

Para deletar caracteres que correspondem a um padrão em Elm, podemos usar a função String.filter. Esta função recebe como argumento uma função que determina se um caractere deve ser mantido ou descartado. Vamos ver um exemplo:

```Elm
let
    string = "Olá, mundo!"
    newString = String.filter (\c -> c /= ' ') string
in
    newString
```

Esse código vai deletar todos os espaços em branco da string "Olá, mundo!", resultando em "Olá,mundo!" como saída.

## Deep Dive

A função String.filter é bastante útil para realizar ações específicas em nossas strings. Podemos usá-la em conjunto com outras funções, como por exemplo String.index ou String.slice para atingir um resultado mais específico. Além disso, fazer a limpeza de dados com esta função pode trazer maior segurança em nossos programas, evitando possíveis vulnerabilidades.

## Veja também

- [Documentação oficial do Elm](https://guide.elm-lang.org/core_language.html)
- [Tutorial de Elm para iniciantes](https://medium.com/@murilomendonca/elm-guia-pr%C3%A1tico-para-iniciantes-e-interessados-bcri%C3%A7%C3%A3o-alem%C3%A3-943574f2cc37)
- [Mais sobre a função String.filter](https://package.elm-lang.org/packages/elm/core/latest/String#filter)