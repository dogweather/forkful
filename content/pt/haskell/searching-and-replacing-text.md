---
title:                "Haskell: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos escrevendo código em Haskell, muitas vezes precisamos fazer alterações em arquivos de texto. Isso pode incluir a correção de uma palavra incorreta ou substituição de uma expressão inteira. Em vez de fazer essas alterações manualmente, podemos utilizar funções de pesquisa e substituição para nos ajudar a economizar tempo e evitar possíveis erros.

## Como fazer

Podemos usar a função `replace` da biblioteca `Data.Text` para realizar a substituição de strings em um arquivo de texto. Veja um exemplo abaixo:

```Haskell
import Data.Text

main = do
    let text = pack "Olá, meu nome é João."
    let newText = replace "João" "Maria" text
    print newText
```
A saída desse código será: "Olá, meu nome é Maria."

Podemos também utilizar expressões regulares para realizar substituições mais complexas. A biblioteca `Text.Regex` nos fornece funções como `subRegex` para realizar essas substituições. Veja um exemplo abaixo:

```Haskell
import Text.Regex

main = do
    let text = "Hoje é dia 01/09/2021."
    let newText = subRegex (mkRegex "[0-9]{2}/[0-9]{2}/[0-9]{4}") text "Hoje é dia 20/03/2022."
    print newText
```
A saída desse código será: "Hoje é dia 20/03/2022."

## Mergulho Profundo

Além das funções básicas de pesquisa e substituição, existem muitas outras opções e estratégias que podemos utilizar para realizar alterações em arquivos de texto. Por exemplo, podemos usar a função `replaceAll` da biblioteca `Text.Regex.TDFA` para substituir todas as ocorrências de uma expressão regular em uma string.

Também podemos fazer uso das funções `find` e `findIndex` da biblioteca `Text` para encontrar e substituir apenas uma parte específica de uma string, em vez de substituir todas as ocorrências.

Ao utilizar expressões regulares, é importante estar ciente de caracteres especiais, como `.` e `+`, e sua correspondência com determinados padrões. Além disso, podemos usar flags para especificar o comportamento da nossa pesquisa, como ignorar letras maiúsculas e minúsculas.

## Veja também

Aqui estão alguns links para mais informações sobre como realizar busca e substituição em Haskell:

- [Documentação oficial de Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Documentação oficial de Text.Regex](https://hackage.haskell.org/package/regex/docs/Text-Regex.html)
- [Tutorial de expressões regulares em Haskell](https://wiki.haskell.org/Regular_expressions)