---
title:                "TypeScript: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por que enviar uma solicitação HTTP em TypeScript?

Enviar solicitações HTTP é uma parte essencial da programação em TypeScript para se comunicar com servidores e obter informações da web. Aprender como enviar essas solicitações pode melhorar significativamente o desempenho e a funcionalidade do seu aplicativo ou site.

## Como enviar uma solicitação HTTP em TypeScript

Para enviar uma solicitação HTTP em TypeScript, você precisará instanciar um objeto `XMLHttpRequest` e usar o método `open()` para especificar o tipo de solicitação e a URL do servidor. Em seguida, você pode usar o método `send()` para enviar a solicitação e capturar a resposta usando `onreadystatechange` ou `onload` para manipulá-la.

Um exemplo de código TypeScript para enviar uma solicitação POST para uma API seria:

```
var xhr = new XMLHttpRequest();
xhr.open('POST', 'https://exemplo-api.com/usuarios', true);
xhr.setRequestHeader('Content-Type', 'application/json');
xhr.onreadystatechange = function () {
  if (xhr.readyState === 4 && xhr.status === 200) {
    console.log("Solicitação enviada com sucesso!");
    console.log("Resposta: " + xhr.responseText);
  }
};
var data = JSON.stringify({
  nome: "João",
  sobrenome: "Silva",
  email: "joao.silva@email.com"
});
xhr.send(data);
```

Este código instancia um objeto `XMLHttpRequest`, define os cabeçalhos necessários, define a função para manipular a resposta e envia os dados necessários na forma de um JSON.

A saída no console seria:

```
Solicitação enviada com sucesso!
Resposta: { "id": 123, "nome": "João", "sobrenome": "Silva", "email": "joao.silva@email.com" }
```

## Detalhando o envio de solicitação HTTP em TypeScript

O exemplo acima é uma introdução básica ao envio de solicitações HTTP em TypeScript. Há muito mais a ser considerado ao enviar solicitações, como definir cabeçalhos de autorização, lidar com erros de resposta e usar bibliotecas como o [`axios`](https://github.com/axios/axios) para simplificar o processo.

Certifique-se de compreender o funcionamento do método `open()`, os diferentes tipos de solicitações que você pode enviar e como manipular a resposta. Estes são elementos cruciais para o envio bem-sucedido de solicitações HTTP em TypeScript.

# Veja também

- [Tutorial oficial do TypeScript sobre solicitações HTTP](https://www.typescriptlang.org/docs/handbook/2/automatic-ref-creation-with-indexed-access-types.html)
- [Documentação oficial do navegador sobre XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)