---
title:                "Imprimindo saída de depuração"
html_title:           "Elm: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Se você é um desenvolvedor de Elm, provavelmente já se deparou com a necessidade de depurar seu código e entender como suas funções estão se comportando. Nesses casos, é extremamente útil utilizar o recurso de impressão de saída de depuração, que permite que você veja os valores das variáveis ​​e dados em tempo de execução.

## Como Fazer

Para imprimir saídas de depuração em Elm, basta utilizar a função `Debug.log`, passando como argumento uma descrição e o valor que você deseja imprimir. Veja um exemplo abaixo:

```
elm
Debug.log "Meu número favorito" 7
```

Isso irá imprimir no console o texto "Meu número favorito" seguido do valor 7, que é o resultado do código. Você pode usar essa função em qualquer lugar dentro do seu código, seja em uma função ou em um termo. Lembre-se apenas de importar o módulo Debug no início do arquivo.

## Mergulho Profundo

É possível utilizar mais de um argumento na função `Debug.log`, permitindo que você imprima vários valores ao mesmo tempo. Além disso, é possível utilizar formatação de string para deixar suas mensagens mais legíveis. Veja um exemplo:

```
elm
Debug.log "Meu número favorito" (toString numero) ++ " é mesmo meu número favorito!"
```

Isso irá imprimir "Meu número favorito 7 é mesmo meu número favorito!", onde 7 é o valor da variável `numero`. Note também que é necessário passar o valor para uma string utilizando a função `toString`.

## Veja Também

- [Documentação do módulo Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Depuração em Elm: dicas e truques](https://dev.to/threewiselegeeks/debugging-in-elm-tips-and-tricks-23o5)