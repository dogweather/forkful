---
title:    "Haskell: Usando expressões regulares"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Haskell?

Expressões regulares são uma ferramenta poderosa e versátil para manipulação de texto em Haskell. Elas permitem que você procure e faça substituições em padrões de texto com facilidade e eficiência. Se você trabalha com processamento de dados ou desenvolvimento web em Haskell, as expressões regulares certamente serão uma parte importante do seu trabalho.

## Como utilizar expressões regulares em Haskell

Para utilizar expressões regulares em Haskell, é necessário importar o módulo `Text.Regex.Posix`. Em seguida, você pode usar a função `=~` para comparar uma string com um padrão de expressão regular. Por exemplo:

```Haskell
"Hello World" =~ "Hello" :: Bool -- retorna True
```

Você pode utilizar caracteres especiais para construir padrões mais complexos, como `[a-z]` para representar qualquer letra minúscula ou `+` para indicar que o caractere anterior pode aparecer uma ou mais vezes. Alguns exemplos de padrões de regex comuns são:

- `[0-9]` para encontrar qualquer dígito numérico
- `[a-zA-Z]` para encontrar qualquer letra, tanto maiúscula quanto minúscula
- `.` para encontrar qualquer caractere
- `*` para indicar que o caractere anterior pode aparecer zero ou mais vezes

Além disso, é possível utilizar parênteses para agrupar padrões e fazer substituições com a função `subRegex`.

## Aprofundando nas expressões regulares em Haskell

Existem diversos recursos avançados para expressões regulares em Haskell, como a utilização de expressões regulares preguiçosas e expressões regulares com lookahead e lookbehind. Além disso, é possível utilizar funções como `matchRegex` e `splitRegex` para obter valores específicos de uma string baseados em um padrão de regex.

É importante lembrar que as expressões regulares são sensíveis a maiúsculas e minúsculas, a menos que você use a opção `IgnoreCase`. Portanto, é preciso estar atento ao utilizar expressões regulares em suas aplicações.

## Veja também

- [Documentação oficial do módulo Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Tutorial de expressões regulares em Haskell](https://www.haskell.org/haskellwiki/Regular_expressions)
- [Site para testar e verificar expressões regulares](https://regexr.com/)