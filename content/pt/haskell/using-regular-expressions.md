---
title:                "Usando expressões regulares"
html_title:           "Haskell: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e por que?

Regular expressions (ou expressões regulares) são sequências de caracteres que definem um padrão de texto para serem procurados e manipulados em uma grande quantidade de dados. É uma ferramenta comum usada por programadores para fazer correspondências complexas de padrões de texto em seus scripts e aplicativos.

## Como fazer:

Os programadores podem usar a biblioteca "Text.Regex.TDFA" do Haskell para trabalhar com expressões regulares. Por exemplo, para verificar se uma string contém um número de telefone válido:

```Haskell
import Text.Regex.TDFA

telefone = "123-456-7890"
padrao = "^\\(?\[0-9]{3}\\)?\[ -\]?\\[0-9]{3}\\[ -\]?\\[0-9]{4}$" :: String

if telefone =~ padrao then
    putStrLn "Número de telefone válido!"
else
    putStrLn "Número de telefone inválido."
```

Saída:
```
"Número de telefone válido!"
```

## Deep Dive:

Expressões regulares têm suas origens na teoria da computação e em linguagens formais, mas se tornaram mais populares com o advento da linguagem Perl na década de 1980. Embora sejam poderosas e úteis, elas podem ser difíceis de ler e entender, especialmente para padrões mais complexos. Existem também outras alternativas para trabalhar com padrões de texto, como parsers combinadores. A implementação geralmente segue o algoritmo de correspondência de padrões de Thompson.

## Veja também:

- [Documentação da biblioteca "Text.Regex.TDFA"](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
- ["Expressões regulares em 5 minutos"](https://www.youtube.com/watch?v=Hztic4Y6ON0)
- [Parsers combinadores no Haskell](https://wiki.haskell.org/Parsing_a_simple_imperative_language)