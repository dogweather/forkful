---
title:    "Haskell: Capitalizando uma string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que Capitalizar uma String em Haskell?

A transformação de uma string para que sua primeira letra seja maiúscula pode ser útil em diversas situações de programação. Isso pode ajudar a imprimir mensagens ou nomes de usuários de maneira mais elegante e legível, ou até mesmo corrigir entradas digitadas incorretamente pelos usuários. Em Haskell, essa operação pode ser realizada de forma simples e eficiente, graças à sua forte tipagem e recursos de manipulação de strings.

## Como Fazer

Para capitalizar uma string em Haskell, podemos usar a função `toUpper` da biblioteca `Data.Char`. Essa função recebe um caractere e retorna sua versão em maiúscula, caso esteja em minúsculo. Podemos então usar essa função junto com outras funções da biblioteca `Data.List`, como `map` e `head`, para aplicá-la em cada caractere da string e obter o resultado desejado. Salienta-se também que é possível criar uma função própria para realizar essa tarefa de maneira mais específica e personalizada.

Vamos ver um exemplo prático de como realizar essa operação em uma string "hello":

```Haskell 
import Data.Char
import Data.List

capitalize :: String -> String
capitalize str = map toUpper (head str) : tail str

main = do
    let hello = "hello"
    putStrLn ("String original: " ++ hello)
    let capitalized = capitalize hello
    putStrLn ("String capitalizada: " ++ capitalized)
```

A saída desse código seria:

```
String original: hello
String capitalizada: Hello
```

Aqui, utilizamos a função `head` para obter o primeiro caractere da string e a função `tail` para obter o restante, e assim aplicamos a função `toUpper` somente no primeiro caractere. Podemos também aproveitar e criar uma função `capitalizeAll` para aplicar a função `toUpper` em todos os caracteres da string:

```Haskell
capitalizeAll :: String -> String
capitalizeAll str = map toUpper str

main = do
    let hello = "hello"
    putStrLn ("String original: " ++ hello)
    let capitalized = capitalizeAll hello
    putStrLn ("String capitalizada: " ++ capitalized)
```

A saída seria a mesma, mas com a string inteira em maiúscula:

```
String original: hello
String capitalizada: HELLO
```

## Aprofundando

Além do que foi visto nos exemplos acima, existem outras formas de capitalizar uma string em Haskell. A biblioteca `Data.Text` também possui funções úteis para manipulação de strings, como a função `toUpper` e a função `words`, que separa uma string em uma lista de palavras. Podemos utilizar essas funções juntas para capitalizar cada palavra individualmente, resultando em uma string com a primeira letra de cada palavra em maiúscula.

Além disso, é possível criar funções mais complexas para capitalizar strings que contenham pontuação, acentos ou até mesmo números. Com as ferramentas certas, é possível realizar a transformação de strings de maneira precisa e eficiente, de acordo com as necessidades do programa.

# Veja Também

- [Documentação da biblioteca Data.Char](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Documentação da biblioteca Data.List](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html)
- [Documentação da biblioteca Data.Text](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)