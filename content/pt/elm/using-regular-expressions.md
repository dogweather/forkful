---
title:                "Usando expressões regulares"
html_title:           "Elm: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são uma ferramenta muito útil para encontrar e manipular padrões de texto em seus programas Elm. Ao aprender a utilizar expressões regulares, você pode economizar tempo e tornar seu código mais eficiente e flexível.

## Como fazer:

```Elm
import Regex exposing (..)

-- Exemplo de expressão regular para encontrar sequência de números
regex = Regex.fromString "\\d+"
-- Função para encontrar e imprimir números em uma string
findNumbers input =
    case regex of
        Ok r ->
            match r input
                |> List.map .match
                |> List.filterMap identity
                |> Debug.log "Números encontrados:"
        Err err ->
            Debug.log "Erro:" err

-- Saída de exemplo: ["123", "456", "789"]
findNumbers "Eu tenho 123 maçãs, 456 laranjas e 789 bananas."
```

Neste exemplo, importamos o módulo `Regex` e criamos uma expressão regular para encontrar sequências de números. Em seguida, utilizamos uma função `findNumbers` para encontrar e imprimir todos os números que correspondem à nossa expressão regular em uma string. Esta é apenas uma das muitas maneiras de utilizar expressões regulares em seus programas Elm.

## Desvendando as expressões regulares:

As expressões regulares podem parecer intimidadoras no início, mas são uma ferramenta poderosa que vale a pena aprender. Algumas coisas importantes a saber incluem:

- Padrões entre barras duplas (`//`) são expressões regulares.
- O caractere `\\` é usado como um escape para caracteres especiais nas expressões regulares, como `+`, `*` e `.`.
- A função `Regex.fromString` transforma uma string em uma expressão regular, retornando um tipo `Result Error Regex`.
- Existem funções úteis no módulo `Regex` como `match` (para verificar se uma string corresponde à expressão regular) e `find` (para encontrar todas as correspondências em uma string).

Se você quiser se aprofundar ainda mais nas expressões regulares, recomendo ler a documentação completa do módulo `Regex` e praticar em exemplos diferentes.

## Veja também:

- [Documentação do módulo Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Tutorial de expressões regulares do MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)