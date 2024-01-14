---
title:    "Haskell: Busca e substituição de texto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que?

Fazer o processo de busca e substituição de texto é uma tarefa comum na programação. Ao aprender a realizar essa função em Haskell, você poderá escrever programas que lidam com manipulação de texto de forma mais eficiente e elegante.

## Como Fazer

Realizar busca e substituição de texto em Haskell é bastante simples. Primeiro, importe o módulo "Data.Text" para poder trabalhar com o tipo de dados "Text". Em seguida, utilize a função "replace" para fazer a substituição.

```
import Data.Text (replace)

textoOriginal = "Olá, mundo!"
novoTexto = replace "Olá" "Hello" textoOriginal

-- Saída: Hello, mundo!
```

Como pode ser visto no exemplo acima, a função "replace" recebe três parâmetros: o texto que será substituído, o texto substituto e o texto original. Caso o texto a ser substituído não seja encontrado no texto original, a função simplesmente retorna o texto original sem modificação.

Para fazer a busca por um padrão em vez de uma string específica, pode-se utilizar a função "replaceWith", que recebe como parâmetros uma função de correspondência e o texto substituto. Por exemplo:

```
import Data.Text (replaceWith)

textoOriginal = "123 ABC"
novoTexto = replaceWith (\_ -> "XYZ") textoOriginal

-- Saída: XYZ XYZ
```

Nesse caso, a função de correspondência apenas ignora o padrão encontrado e retorna sempre o texto substituto.

## Aprofundando

Ao trabalhar com strings, é importante ter em mente que elas são imutáveis em Haskell. Isso significa que toda vez que uma substituição é feita, um novo texto é criado em vez de modificar o texto original. Isso pode gerar uma sobrecarga de memória em programas que realizam múltiplas substituições.

Para lidar com isso, é possível utilizar a função "foldl'" em conjunto com a função "replaceWith". Essa função realiza uma operação em cada elemento de uma lista e retorna o resultado final. No contexto de busca e substituição de texto, pode-se utilizar essa função para aplicar várias substituições em um mesmo texto sem criar múltiplos textos intermediários. Um exemplo seria:

```
import Data.Text (replaceWith)
import qualified Data.Text.Foldable as TF

textoOriginal = "ABC 123 D"
novoTexto = TF.foldl' (\str pattern -> replaceWith (\_ -> "X") str) textoOriginal ["ABC", "123", "D"]

-- Saída: X X X
``` 

## Veja Também

- [Documentação oficial do módulo Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Tutorial de busca e substituição de texto em Haskell](https://www.codewars.com/kata/search-and-replace-text-in-a-string-haskell/train/haskell)
- [Funcionalidades do módulo Data.Text](https://mmhaskell.com/haskell-strings-and-text)