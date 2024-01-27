---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Usar expressões regulares é sobre procurar padrões em texto. Programadores as utilizam para validar, buscar, e manipular dados de forma eficiente.

## Como Fazer:
Elm usa o pacote `regex` para trabalhar com expressões regulares. Vamos a alguns exemplos:

```Elm
import Regex

-- Verificando se um padrão existe no texto
isMatch : Regex -> String -> Bool
isMatch =
    Regex.contains

-- Uso
exampleIsMatch : Bool
exampleIsMatch =
    isMatch (Regex.fromString "^[a-zA-Z]+$" |> Maybe.withDefault Regex.never) "ElmLang"

-- Substituindo texto com padrão
replace : Regex -> String -> String -> String
replace pattern replacement text =
    Regex.replace pattern (\_ -> replacement) text

-- Uso
exampleReplace : String
exampleReplace =
    replace (Regex.fromString "\\d+" |> Maybe.withDefault Regex.never) "NUM" "Elm 0.19.1"
```

Output para `exampleIsMatch`:
```
True
```

Output para `exampleReplace`:
```
"Elm NUM.NUM"
```

## Aprofundando
Expressões regulares existem há décadas, sendo parte integrante de várias linguagens de programação. Alternativas incluem parsers dedicados ou usar funções de string específicas, mas expressões regulares muitas vezes são mais rápidas e mais versáteis. No Elm, você trabalha com elas através do pacote `regex`, que usa o `Maybe` para lidar com padrões inexistentes de forma segura.

## Veja Também

- Documentação oficial do pacote `regex`: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Tutorial interativo de regex: [regexone.com](https://regexone.com)
