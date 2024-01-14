---
title:                "Elm: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que Concatenar Strings em Elm?

Se você já trabalhou com programação em Elm, provavelmente já viu a necessidade de juntar duas ou mais strings em um único valor. Isso é conhecido como "concatenação de strings" e é uma operação comum em muitos projetos. Neste artigo, vamos explorar por que essa operação é tão importante e como fazê-la de forma eficiente em Elm.

## Como Fazer a Concatenação de Strings em Elm?

A concatenação de strings em Elm é bastante simples e pode ser feita de várias maneiras. A forma mais básica é usando o operador `++`, que une duas strings em uma só. Veja um exemplo abaixo:

```Elm
nome = "João"
sobrenome = "Silva"

nomeCompleto = nome ++ " " ++ sobrenome

-- Output: João Silva
```

Além disso, você também pode usar a função `String.concat`, que permite unir uma lista de strings em uma só. Veja um exemplo:

```Elm
listaNomes = ["João", "Maria", "José"]
nomesConcatenados = String.concat listaNomes

-- Output: JoãoMariaJosé
```

É importante lembrar que, em Elm, as strings são imutáveis, ou seja, elas não podem ser alteradas depois de criadas. Portanto, sempre que você fizer uma concatenação, uma nova string será criada.

## Mergulhando Mais Fundo na Concatenação de Strings

Uma das vantagens de usar a função `String.concat` é que ela é otimizada para concatenar várias strings de uma vez. Isso significa que, quanto maior a lista de strings, mais eficiente será a concatenação. Já usando o operador `++`, cada concatenação criará uma nova string e será menos eficiente para listas grandes.

Também é importante lembrar que a concatenação de strings não se limita apenas a juntar palavras ou frases. É possível concatenar qualquer tipo de dado que possua uma representação como string, como números e booleanos.

## Veja Também

- [Documentação oficial do Elm sobre Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial sobre Strings em Elm](https://www.elm-tutorial.org/pt-br/03-strings.html)
- [Exemplos práticos de concatenação de strings em Elm](https://www.elmbytes.com/posts/concatenating-strings-in-elm)

Esperamos que este artigo tenha sido útil para entender a importância e as diferentes maneiras de fazer a concatenação de strings em Elm. Continue explorando o mundo da programação funcional com Elm e divirta-se criando projetos incríveis!