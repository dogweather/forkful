---
title:    "Haskell: Encontrando o comprimento de uma string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string é importante?

Encontrar o comprimento de uma string é uma habilidade fundamental na programação. Saber quantos caracteres uma string contém pode ser útil em diversas situações, como validar entradas de usuário, manipulação de dados e formatação de saída.

## Como fazer:

```Haskell
-- Definindo uma função para encontrar o comprimento de uma string
findLength :: String -> Int 
findLength str = length str

-- Exemplos de entrada e saída da função
findLength "Olá" -- output: 3
findLength "123456789" -- output: 9
```

A função `length` é pré-definida na linguagem Haskell e retorna o comprimento de qualquer estrutura que possa ser percorrida, como strings, listas e arrays. Entretanto, é importante ressaltar que o comprimento de uma string pode ser diferente do número de caracteres visíveis, já que caracteres unicode possuem diferentes tamanhos.

## Profundidade:

Ao encontrarmos o comprimento de uma string, estamos na verdade contando quantos elementos ela possui. Em Haskell, o tipo `String` é equivalente a uma lista de caracteres `Char`, então podemos utilizar a função `length` para encontrar tanto o comprimento de uma string quanto de uma lista. É importante salientar que o tipo `String` é uma lista com elementos do tipo `Char`, então manipulações e operações que funcionam com listas podem ser aplicadas em strings também.

Outro detalhe importante é que a função `length` é uma função pura, ou seja, ela não possui efeitos colaterais e sempre retorna o mesmo resultado para uma mesma entrada. Isso significa que seu código será mais previsível e menos propenso a erros.

## Veja também:

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Funções pré-definidas no Haskell](https://wiki.haskell.org/Standard_functions)
- [Tutorial interativo de Haskell](https://learnyouahaskell.com/chapters)
- [Documentação da linguagem Markdown](https://www.markdownguide.org/)