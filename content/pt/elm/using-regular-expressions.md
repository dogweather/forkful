---
title:                "Utilizando expressões regulares"
html_title:           "Elm: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e Porque?

Expressões regulares são padrões utilizados para identificar texto em uma determinada sequência de caracteres. Elas são utilizadas por programadores para encontrar e manipular informações em grande quantidade de dados. Com elas, é possível realizar validações em campos de formulários, buscar informações específicas em um texto, entre outros usos.

## Como Fazer:

```elm
string = "O dia está ensolarado!"
regex = RegExp.fromPattern "dia.*ensolarado" False False
RegExp.match regex string
```

Saída:

```
["dia está ensolarado"]
```

## Explorando Mais:

As expressões regulares foram inventadas por Stephen Coleman Kleene na década de 1950 e desde então se tornaram amplamente utilizadas em diferentes linguagens de programação. Existem opções de bibliotecas para lidar com expressões regulares em Elm, como a RegExp, que faz parte da biblioteca "elm/parser". Algumas alternativas às expressões regulares incluem a utilização de funções de manipulação de strings, porém, essas podem ser mais limitadas em termos de funcionalidade. No Elm, expressões regulares são implementadas de forma eficiente com o suporte do motor de busca Thompson NFA.

## Veja Também:

- Aprenda mais sobre expressões regulares em [regexone.com](https://regexone.com/)
- Conheça a biblioteca para expressões regulares em Elm em [package.elm-lang.org](https://package.elm-lang.org/packages/elm/parser/latest)