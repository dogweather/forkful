---
title:                "Enviando uma solicitação http"
html_title:           "Fish Shell: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e porquê?
Enviar uma solicitação HTTP é uma forma de comunicação entre um aplicativo e um servidor. Os programadores usam isso para obter informações de um servidor ou atualizar informações enviadas anteriormente.

## Como fazer:
```
Fish Shell
# Enviar solicitação GET para uma URL específica
curl -X GET URL

# Enviar solicitação POST com dados JSON para uma URL específica
curl -X POST -H "Content-Type: application/json" -d '{"username": "exemplo", "password": "senha"}' URL
```

## Deep Dive:
Anteriormente, os programadores geralmente usavam a ferramenta de linha de comando cURL para enviar solicitações HTTP. No entanto, com o Fish Shell, isso pode ser feito diretamente no shell.

Uma alternativa para o Fish Shell é o cURL Shell, que oferece recursos adicionais para o uso do cURL.

O processo de envio de solicitações HTTP no Fish Shell é feito usando o comando `curl`. Ele aceita vários parâmetros, como o tipo de solicitação (GET, POST, PUT) e os cabeçalhos (H) e dados (d) necessários para a solicitação.

## Ver também:
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [cURL Shell](https://github.com/mrmuino/curl-sh)