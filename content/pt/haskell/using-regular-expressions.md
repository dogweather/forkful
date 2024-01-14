---
title:                "Haskell: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Haskell

As expressões regulares são uma forma poderosa de realizar operações de busca e manipulação de strings em qualquer linguagem de programação. No caso do Haskell, elas podem ser usadas para simplificar tarefas comuns em manipulação de texto, como a validação de formatos de dados ou a extração de informações específicas de uma string.

## Como utilizar expressões regulares em Haskell

Para utilizar expressões regulares em Haskell, primeiro é necessário importar o módulo `Text.Regex.Posix`, que contém funções e tipos necessários para trabalhar com expressões regulares. Veja um exemplo de código a seguir:

```Haskell
import Text.Regex.Posix

-- Verifica se uma string corresponde a um determinado padrão
matchesPattern :: String -> Bool
matchesPattern str = str =~ "[a-zA-Z]+" :: Bool

main :: IO()
main = do
    let text = "escrevendo em português"
    let result = matchesPattern text
    print result -- True
```

No exemplo acima, utilizamos o operador `=~` para verificar se a string `text` corresponde ao padrão `[a-zA-Z]+`, que representa uma ou mais letras maiúsculas ou minúsculas. O resultado é armazenado em uma variável e impresso na tela.

## Aprofundando no uso de expressões regulares

Expressões regulares podem ser utilizadas de diversas maneiras em Haskell, sendo possível realizar tanto operações básicas de busca e substituição quanto operações mais complexas, como a extração de dados específicos de uma string.

Além disso, é possível utilizar expressões regulares com diferentes tipos de dados em Haskell, como `String`, `ByteString` e `Text`. Cada tipo possui suas próprias funções e métodos para manipulação de expressões regulares.

Para aprofundar seus conhecimentos sobre o uso de expressões regulares em Haskell, é recomendável estudar a documentação do módulo `Text.Regex.Posix` e praticar com diferentes exemplos.

## Veja também

- [Documentação oficial do módulo Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Tutorial sobre expressões regulares em Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/text-manipulation/regular-expressions)
- [Vídeo aula sobre expressões regulares em Haskell](https://www.youtube.com/watch?v=ttP-En0WKTY)