---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar um pedido HTTP é um modo de se comunicar com um servidor web. Este é um passo crucial para a utilização de APIs, a recolha e envio de dados, ou para interagir com uma página web.

## Como Fazer:

Para enviar um pedido HTTP em PHP, nós podemos utilizar a biblioteca cURL. Aqui está um exemplo de um GET request:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'https://api.meusite.com.br');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

$output = curl_exec($ch);

curl_close($ch);      
?>
```
A resposta do servidor será armazenada na variável `$output`. Agora, se quisermos enviar um POST request, podemos fazer o seguinte:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'https://api.meusite.com.br');
curl_setopt($ch, CURLOPT_POST, 1);
curl_setopt($ch, CURLOPT_POSTFIELDS, 'chave1=valor1&chave2=valor2');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

$output = curl_exec($ch);

curl_close($ch);      
?>
```
Aqui, os dados são enviados no body do pedido HTTP.

## Aprofundando

O protocolo HTTP foi concebido em 1989 pelo cientista britânico Tim Berners-Lee. Desde então, ele se tornou a base para qualquer transferência de dados na web.

Existem muitas alternativas para o PHP cURL, como o Guzzle, um cliente HTTP extenso e poderoso, ou o comando `file_get_contents()` do PHP, que é mais simples mas não tão flexível quanto cURL.

Ao enviar um pedido HTTP, é importante salientar que existem diferentes tipos de pedidos - GET, POST, PUT, DELETE, entre outros – que correspondem a diferentes operações que você pode querer realizar num servidor web.

## Ver Também

- Documentação oficial do PHP cURL: https://www.php.net/manual/pt_BR/book.curl.php
- Guia para pedidos HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Guia_ao_HTTP
- Documentação oficial do Guzzle: http://docs.guzzlephp.org/en/stable/
- Como usar o `file_get_contents()`: https://php.net/manual/en/function.file-get-contents.php