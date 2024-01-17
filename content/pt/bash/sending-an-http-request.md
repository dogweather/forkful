---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Enviar uma requisição HTTP é quando um programa envia uma mensagem para um servidor, solicitando uma ação ou informação. Programadores fazem isso para interagir com APIs, obter dados de um servidor, ou enviar informações.

## Como fazer:

Para enviar uma requisição HTTP, você pode usar o comando curl no terminal Bash. Por exemplo: 

```
curl -X GET https://www.example.com
```

Isso enviará uma requisição GET para o servidor em www.example.com e retornará o código de resposta, cabeçalhos e corpo da resposta.

## Mergulho Profundo:

O protocolo HTTP foi criado em 1991 e tem sido amplamente utilizado para comunicação entre computadores em redes. Existem outras ferramentas de linha de comando, como o wget, que também podem ser usadas para enviar requisições HTTP. A implementação de uma requisição HTTP pode envolver adicionar cabeçalhos específicos, como Content-Type e Authorization, para autenticar e enviar conteúdo ao servidor.

## Veja também:

- Documentação do cURL: https://curl.se/docs/
- Introdução ao HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview