---
title:                "Elm: Procurando e substituindo texto"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Há muitas situações em que precisamos fazer alterações em grande quantidade de texto. Pode ser para corrigir erros ou para padronizar trechos de código. Independente da razão, saber como pesquisar e substituir texto é uma habilidade importante para qualquer programador.

## Como Fazer

### Usando o ReplaceAll da String

Uma forma simples de realizar substituições de texto em Elm é utilizando a função replaceAll da biblioteca String. Ela recebe três argumentos: a substring a ser substituída, a substring de substituição e a string em que a substituição será feita.

```elm
import String exposing (replaceAll)

texto = "Bem-vindo ao mundo da programação"
novoTexto = replaceAll "programação" "codificação" texto

-- o resultado será: "Bem-vindo ao mundo da codificação"
```

### Usando Regex

Outra opção é utilizar expressões regulares (ou regex) para realizar as substituições. A biblioteca Regex expõe funções úteis para trabalhar com esse tipo de padrão.

```elm
import Regex exposing (replace, regex)

texto = "Muito Obrigado!"
novoTexto = replace (regex "[A-Z]") (\_ -> "!") texto

-- o resultado será: "muito obrigado!"
```

## Uma Mergulho Mais Profundo

Existem diversas outras maneiras de realizar buscas e substituições de texto em Elm, como utilizando as funções map, filter e fold do módulo List, ou ainda criando uma função de substituição personalizada. Além disso, é importante lembrar que a biblioteca String possui várias outras funções interessantes para trabalhar com texto.

## Veja Também

- [Documentação da Biblioteca String em Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Artigo sobre Expressões Regulares em Elm (em inglês)](https://thoughtbot.com/blog/elm-regular-expressions)
- [Exemplos de Regex em Elm para exercitar](https://alfredomotta.com/regular-expressions-in-elm.html)