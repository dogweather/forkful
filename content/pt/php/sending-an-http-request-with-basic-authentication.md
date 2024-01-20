---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que é e Por quê?

O envio de um pedido HTTP com autenticação básica é uma forma de garantir que apenas os usuários autorizados possam acessar determinados dados em seu programa. Os programadores usam isso para proteger informações sensíveis e manter a segurança do aplicativo.

## Como fazer:
Vamos começar com um exemplo básico de como enviar um pedido HTTP com autenticação básica usando PHP.

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL,"http://meusite.com/dados");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_USERPWD, 'usuario:senha'); 
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

$output = curl_exec($ch);

if (curl_errno($ch)) {
    echo 'Falha: ' . curl_error($ch);
} else {
    echo 'Sucesso:' . $output;
}

curl_close($ch);
?>
```
Aqui, estamos usando a função cURL do PHP. Primeiro, inicializamos o pedido cURL com `curl_init()`. Em seguida, definimos várias opções para o pedido usando `curl_setopt()`. Isso inclui o URL do pedido, as credenciais de autenticação e o tipo de autenticação (neste caso, BASIC).

## Mergulho profundo

A autenticação HTTP básica é uma técnica antiga, mas ainda amplamente utilizada. No entanto, ela não é quebra a segurança, já que as credenciais são enviadas como texto claro (base64 codificado e não criptografado). Por causa disso, é sempre recomendado usar HTTPS quando estiver trabalhando com autenticação básica.

Existem outras formas de autenticação disponíveis também, como OAuth, que pode oferecer maior segurança, mas são mais complexas de implementar.

Em PHP, a maneira mais comum de enviar uma solicitação HTTP com autenticação básica é usando a biblioteca cURL, como mostrado acima. No entanto, você também pode usar outras bibliotecas como Guzzle ou até mesmo a extensão HTTP nativa do PHP.

## Ver também

Para obter mais informações, confira os seguintes recursos:

- Documentação oficial do PHP sobre a biblioteca cURL: (https://www.php.net/manual/pt_BR/book.curl.php)
- Autenticação com OAuth em PHP: (https://oauth.net/code/php/)
- Biblioteca Guzzle para PHP: (http://docs.guzzlephp.org/)
- Extensão PHP HTTP pecl: (https://pecl.php.net/package/pecl_http)
- Autenticação HTTP Básica: (https://tools.ietf.org/html/rfc7617)