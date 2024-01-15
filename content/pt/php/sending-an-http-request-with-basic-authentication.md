---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "PHP: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica é uma maneira simples e eficiente de proteger dados e recursos em uma aplicação web. Com a autenticação básica, o usuário precisa fornecer um nome de usuário e uma senha para acessar conteúdos ou recursos específicos.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em PHP, você precisará utilizar a função `curl_setopt()` e definir as opções `CURLOPT_USERPWD` e `CURLOPT_HTTPAUTH`. Em seguida, você deve fornecer o nome de usuário e senha desejados como uma string concatenada no formato "usuário:senha". Veja um exemplo de código abaixo:

```
<?php
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://exemplo.com.br/api/recurso");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_USERPWD, "seu_usuario:sua_senha");
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
$resultado = curl_exec($ch);

echo $resultado;
?>
```

Caso deseje verificar se a solicitação foi bem-sucedida, você pode utilizar a função `curl_getinfo()` para retornar informações sobre a requisição. O código de resposta 200 indica que a autenticação básica foi realizada com sucesso.

## Aprofundando-se

Ao utilizar a autenticação básica, é importante lembrar que as credenciais de usuário não são criptografadas durante a transmissão. Isso significa que se alguém conseguir interceptar a solicitação, poderá ter acesso ao nome de usuário e senha do usuário. Por isso, é recomendado utilizar uma conexão HTTPS para proteger as informações durante a transmissão.

Além disso, é importante armazenar as credenciais de usuário com segurança no servidor para garantir a integridade dos dados. Evite armazená-las em arquivos de texto ou em banco de dados sem criptografia.

## Veja também

- [Documentação oficial do PHP sobre a função curl_setopt()](https://www.php.net/manual/pt_BR/function.curl-setopt.php)
- [Artigo sobre autenticação básica em aplicações web](https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-nginx-server-blocks-pt)