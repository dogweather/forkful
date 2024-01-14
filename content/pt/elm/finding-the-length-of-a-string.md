---
title:    "Elm: Encontrando o tamanho de uma string."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você pode ter se perguntado algum dia: "Por que eu preciso saber o comprimento de uma string?". Embora possa parecer uma tarefa simples, essa habilidade é essencial para executar muitas tarefas de programação, como validar entradas de usuário, manipular dados e criar códigos eficientes.

## Como fazer

Para encontrar o comprimento de uma string em Elm, basta usar a função `String.length`. Veja um exemplo abaixo:

```elm
nome = "Maria"
comprimento = String.length nome

-- Output: 5
```

Neste exemplo, criamos uma variável chamada `nome` que armazena uma string e, em seguida, usamos a função `String.length` para encontrar o comprimento da string e atribuí-lo à variável `comprimento`.

## Deep Dive

Para entender melhor como a função `String.length` funciona, é importante entender que os caracteres em uma string são contados a partir do índice 0. Isso significa que o primeiro caractere é representado pelo número 0, o segundo pelo número 1 e assim por diante.

Portanto, quando usamos `String.length`, ele conta o número de caracteres em uma string e retorna esse valor, que neste exemplo é 5.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre strings em Elm:

- [Documentação da função String.length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- [Tutorial sobre strings em Elm](https://guide.elm-lang.org/core_language.html#strings)
- [Guia de referência para a linguagem Elm](https://elm-lang.org/docs)

Agora que você sabe como encontrar o comprimento de uma string em Elm, pode aplicar essa habilidade em seus próprios projetos e tornar seus códigos mais eficientes. Divirta-se codando!