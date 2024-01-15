---
title:                "Escrevendo para o erro padrão"
html_title:           "Javascript: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para o erro padrão (standard error) pode ser muito útil ao desenvolver aplicações em Javascript. Esta técnica permite que você identifique erros rapidamente e melhore a depuração de seu código.

## Como

Para escrever para o erro padrão em Javascript, você pode usar a função "console.error()", que leva uma mensagem como argumento. Por exemplo:

```Javascript
console.error("Opa, algo deu errado!");
```

Isso irá imprimir a mensagem no console e você poderá ver a informação de erro ao testar seu código. O output seria algo como:

```
Opa, algo deu errado!
```

## Deep Dive

Escrever para o erro padrão também pode ser útil ao lidar com exceções e erros no seu código. Por exemplo, se você estiver trabalhando com APIs, pode usar o "console.error()" para imprimir mensagens de erro personalizadas quando receber uma resposta inválida.

Além disso, é possível customizar a formatação da mensagem de erro, adicionando variáveis e outras informações relevantes para facilitar a identificação e correção do problema.

## Veja também

Aqui estão alguns recursos adicionais que podem ajudá-lo a se aprofundar no assunto:

- [Documentação oficial sobre console.error()](https://developer.mozilla.org/pt-BR/docs/Web/API/Console/error)
- [Artigo sobre depuração de código em Javascript](https://blog.bitsrc.io/mastering-the-art-of-debugging-javascript-with-console-api-36e3b1676f5f)
- [Vídeo explicando como usar console.error() para depuração em tempo real](https://www.youtube.com/watch?v=gnkrDse9QKc)