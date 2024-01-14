---
title:                "Haskell: Encontrando o comprimento de uma string."
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação. Saber como fazer isso pode ser útil em uma variedade de cenários, desde a manipulação de dados até a formatação de saída. Além disso, é um bom exercício para fortalecer suas habilidades em Haskell.

## Como fazer

A função `length` em Haskell pode ser usada para encontrar o comprimento de uma string. Aqui está um exemplo simples de como usá-la:

```Haskell
main = do
    let str = "Olá, mundo!"
    putStrLn ("O comprimento da string é: " ++ (show (length str)))
```

A saída deste código será `O comprimento da string é: 12`, indicando que a string possui 12 caracteres. Agora, vamos ver um exemplo um pouco mais complexo que envolve a entrada do usuário:

```Haskell
main = do
    putStrLn "Digite uma palavra:"
    str <- getLine
    putStrLn ("A palavra " ++ str ++ " tem " ++ (show (length str)) ++ " letras.")
```

Ao executar este código e digitar uma palavra, a saída será algo como `A palavra casa tem 4 letras.` Isso mostra como a função `length` pode ser usada em conjunto com outras funções e entrada do usuário para obter resultados úteis.

## Mergulho profundo

Embora a função `length` seja bastante simples, existem algumas coisas interessantes por trás dela. Por exemplo, ela pode ser usada em listas de outros tipos de dados, como números ou booleanos. Além disso, a função é implementada de forma recursiva em Haskell, o que pode ser um bom ponto de discussão para entender melhor como ela funciona.

## Veja também

- [Tutorial de Haskell](https://www.haskell.org/tutorial/)
- [Documentação da função length](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length)
- [Exercícios práticos de programação em Haskell](http://www.inf.ufg.br/~eden/teaching/plc/Lista-exercicios-Haskell.pdf)