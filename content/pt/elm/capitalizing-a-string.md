---
title:                "Capitalizando uma string"
html_title:           "Elm: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Existem várias razões pelas quais alguém pode querer capitalizar uma string em seu código Elm. Pode ser para padronizar a formatação de strings em um projeto, para melhorar a legibilidade do código ou para atender a requisitos específicos de formatação em uma aplicação.

## Como fazer

Para capitalizar uma string em Elm, podemos utilizar a função `String.toUpper`, que converte todos os caracteres de uma string para letras maiúsculas.

```
Elm
String.toUpper "exemplo" 
-- Resultado: "EXEMPLO"
```

Também podemos utilizar a função `String.toTitle`, que capitaliza apenas a primeira letra de cada palavra em uma string.

```
Elm
String.toTitle "exemplo de frase" 
-- Resultado: "Exemplo De Frase"
```

É importante lembrar que, em Elm, strings são imutáveis, ou seja, não podemos alterá-las diretamente. Portanto, é necessário atribuir o resultado da função a uma nova variável ou utilizá-lo em uma expressão `let` no nosso código.

## Aprofundando

Além das funções mencionadas acima, existem outras formas de capitalizar strings em Elm. Por exemplo, podemos utilizar a biblioteca `elm-community/string-extra` para ter acesso a funções como `upperCaseFirst` e `titleCase`, que oferecem mais opções de formatação de strings.

Também é importante ressaltar que essas funções utilizam regras linguísticas específicas para cada idioma, o que pode afetar o resultado final em determinadas situações. Por isso, é sempre válido verificar se a formatação está correta antes de usar essas funções em um projeto.

## Veja também

- Documentação da biblioteca `elm/core`: https://package.elm-lang.org/packages/elm/core/latest/String
- Biblioteca `elm-community/string-extra`: https://package.elm-lang.org/packages/elm-community/string-extra/latest/