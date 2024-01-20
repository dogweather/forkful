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

## O Quê & Porquê?

Enviar um pedido HTTP é basicamente solicitar e obter dados de um servidor web. Programadores fazem isso para interagir com APIs, pegar informações e, em alguns casos, enviar dados para serem processados.

## Como fazer:

Usaremos o comando `curl` em Bash para enviar um pedido HTTP GET. Aqui está um exemplo que pega dados do servidor web example.com.

```Bash
$ curl http://www.example.com
```
Output:
```Bash
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

Para enviar um pedido POST, podemos adicionar mais opções ao comando `curl`.

```Bash
$ curl -X POST -d "username=user&password=pass" http://www.example.com/login
```

## Mergulho Profundo

O protocolo HTTP foi introduzido em 1991 e é o pilar da comunicação de dados na web. Alternativamente, você pode utilizar `wget` ao invés de `curl` para enviar pedidos HTTP em bash. `curl` oferece mais opções e flexibilidade, mas `wget` é mais fácil para casos de uso simples.

O `curl` é na verdade uma biblioteca e uma ferramenta de linha de comando. Ele suporta uma variedade de protocolos além de HTTP, incluindo FTP, IMAP, POP3 e outros. Quando você envia um pedido HTTP, o `curl` cria uma conexão de socket com o servidor, envia um texto formatado representando o pedido e, em seguida, lê a resposta.

## Ver Também

- Documentação oficial do cURL: https://curl.se/doc/
- Tutorial de `wget`: https://www.gnu.org/software/wget/manual/wget.html
- API REST explicada: https://codewithmosh.com/p/build-web-apis-with-asp-net-and-entity-framework