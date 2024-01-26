---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Regular expressions, ou regex, são padrões usados para encontrar correspondências de texto. Programadores utilizam regex para busca e substituição de texto, validação de dados e análise sintática de cadeias de caracteres.

## Como Fazer:

Em Haskell, você pode usar o pacote `regex-posix` para trabalhar com regex. Aqui estão exemplos de código e saída mostrando como usar regex para encontrar correspondências de texto:

```haskell
import Text.Regex.Posix

-- Verifica se uma string corresponde a um padrão regex
corresponde :: String -> String -> Bool
corresponde texto padrão = texto =~ padrão :: Bool

main :: IO ()
main = do
    print $ "123abc" `corresponde` "[a-zA-Z]+"
    print $ "abc" `corresponde` "[a-zA-Z]+"
```

Saída:

```
False
True
```

Encontrando todas as correspondências em uma string:

```haskell
-- Encontra todas as correspondências
findMatches :: String -> String -> [String]
findMatches texto padrão = texto =~ padrão :: [String]

main :: IO ()
main = do
    print $ findMatches "a1 b2 c3" "[a-zA-Z]"
```

Saída:

```
["a","b","c"]
```

## Visão Detalhada:

O uso de regex em Haskell é suportado por vários pacotes, como `regex-posix` que segue POSIX regex, e `regex-pcre`, que oferece compatibilidade com a biblioteca PCRE. Regex surgiu nos anos 1950 com o matemático Stephen Kleene e evoluiu significativamente desde então. Alternativas para regex incluem parsers combinator e linguagens específicas de domínio (DSLs), que podem oferecer maior clareza e desempenho em determinados contextos. Internamente, regex é implementado utilizando automatos finitos para análise e correspondência de padrões.

## Veja Também:

- Documentação do regex-posix: [Hackage: regex-posix](https://hackage.haskell.org/package/regex-posix)
- Tutorial Haskell sobre regex: [HaskellWiki: Regular expressions](https://wiki.haskell.org/Regular_expressions)
- Comparação de regex e parser combinators: [Stack Overflow: Regex vs. Parser Combinators](https://stackoverflow.com/questions/30734615/regex-vs-parser-combinators)
