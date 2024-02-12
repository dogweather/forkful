---
title:                "Enviando uma requisição HTTP"
aliases:
- /pt/javascript/sending-an-http-request/
date:                  2024-01-20T18:00:15.021493-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar uma requisição HTTP é o processo de solicitar ou enviar dados a um servidor web. Programadores fazem isso para interagir com APIs, recuperar dados para aplicativos ou enviar informações de formulários.

## Como Fazer:
Vamos usar o `fetch`, que é uma forma moderna e fácil de enviar requisições HTTP em JavaScript:

```javascript
// GET request para pegar dados
fetch('https://api.exemplo.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Erro ao buscar dados:', error));

// POST request para enviar dados
fetch('https://api.exemplo.com/submit', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    nome: 'João',
    mensagem: 'Olá, mundo!'
  }),
})
  .then(response => {
    if (!response.ok) {
      throw new Error('Problema na requisição: ' + response.status);
    }
    return response.json();
  })
  .then(data => console.log('Sucesso:', data))
  .catch(error => console.error('Erro ao enviar dados:', error));
```

Saída possível de um `console.log` seria um objeto JSON com os dados solicitados ou uma mensagem de sucesso.

## Mergulho Profundo
Enviar requisições HTTP é fundamental para a web moderna. O `XMLHttpRequest` foi o avô das chamadas AJAX. Mas ele era complicado de usar e pouco intuitivo. O `fetch` chegou com a promessa de uma API nativa, promessas incorporadas (sim, trocadilho intencional) e um fluxo de controle mais simples.

Alternativas não faltam: bibliotecas como Axios, jQuery (sim, ainda é usada) ou o novo e brilhante Axios. Mas o `fetch` é nativo e, em geral, é tudo que você precisa.

Quanto aos detalhes, ao usar `fetch`, você está fazendo uma promessa: uma operação que acontecerá eventualmente. É por isso que usamos `.then()` para lidar com a resposta quando estiver pronta. E a beleza do `fetch` é que ele trata tanto de requisições `GET` quanto `POST`, `PUT`, `DELETE`, etc.

## Veja Também
- MDN Web Docs sobre `fetch`: https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API
- “You Don’t Need Axios”: https://danlevy.net/you-may-not-need-axios/
- JavaScript Promises: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Using_promises
