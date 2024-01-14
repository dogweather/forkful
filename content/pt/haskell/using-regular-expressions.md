---
title:                "Haskell: Usando expressões regulares"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Haskell?

Expressões regulares são uma ferramenta poderosa para lidar com padrões de texto em linguagens de programação. Usar expressões regulares em Haskell pode permitir que você encontre rapidamente determinados padrões em uma string, substitua ou extraia partes específicas do texto e muito mais. Se você está trabalhando com dados de texto em um programa Haskell, é uma boa ideia ter pelo menos um conhecimento básico de expressões regulares.

## Como usar expressões regulares em Haskell

Para usar expressões regulares em Haskell, você precisará importar o módulo "Text.Regex.Posix". Em seguida, use a função `makeRegex` para criar um objeto Regex com base em uma string de expressão regular. Por exemplo:

```Haskell
import Text.Regex.Posix

texto = "Olá, mundo!"
regex = makeRegex "mundo" :: Regex
match = matchTest regex texto -- match será True
```

Você também pode usar a função `match` para encontrar todas as correspondências de uma expressão regular em uma string. Por exemplo:

```Haskell
import Text.Regex.Posix

texto = "Eu tenho 3 maçãs e 2 laranjas."
regex = makeRegex "[0-9]+"
match = matchAll regex texto -- match será ["3", "2"]
```

## Aprofundando nas expressões regulares em Haskell

Existem muitas funcionalidades diferentes de expressões regulares que podem ser usadas em Haskell, incluindo metacaracteres, grupos de captura, substituições e muito mais. Para aprender mais sobre expressões regulares em Haskell, consulte a documentação do módulo "Text.Regex.Posix" ou outros recursos online.

## Veja também

- Documentação do módulo Text.Regex.Posix: https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix.html
- Tutorial de expressões regulares em Haskell: https://wiki.haskell.org/Regular_expressions