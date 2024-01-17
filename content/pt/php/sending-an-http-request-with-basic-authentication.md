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

## O que & porquê?
Enviar uma solicitação HTTP com autenticação básica é um processo que permite que os programadores façam uma requisição para um servidor com um par de credenciais básicas, geralmente um nome de usuário e uma senha. Isso é feito para verificar a identidade do usuário e permitir o acesso a determinados recursos protegidos. Programadores fazem isso para garantir a segurança das suas aplicações e controlar o acesso dos usuários a recursos sensíveis.

## Como fazer:
```php
<?php
$username = "exemplo";
$password = "segredo";

$url = "https://www.exemplo.com/api/dados";

// Cria um contexto com as credenciais básicas
$context = stream_context_create([
    'http' => [
        'method' => 'GET',
        'header' => 'Authorization: Basic ' . base64_encode($username . ':' . $password)
    ]
]);

// Envia a requisição com o contexto criado
$response = file_get_contents($url, false, $context);

// Imprime o resultado da requisição
echo $response;
```

O código acima mostra como enviar uma solicitação HTTP com autenticação básica utilizando a função `file_get_contents()` do PHP. O parâmetro `header` é utilizado para adicionar o cabeçalho de autorização com as credenciais codificadas em base64. O resultado da requisição é armazenado na variável `$response` e pode ser usado da forma que for necessário.

## Deep Dive:
A autenticação básica é um método de autenticação amplamente utilizado na web, sendo suportado pela maioria dos servidores e navegadores. No entanto, possui algumas vulnerabilidades de segurança, como a possibilidade de interceptação das credenciais durante a comunicação. Por esse motivo, é recomendado o uso de protocolos de segurança mais robustos, como a autenticação OAuth.

Existem diversas formas de enviar uma solicitação HTTP com autenticação básica em PHP, como através da função `curl_exec()` ou criando manualmente o cabeçalho de autorização no código. Cada método possui suas vantagens e desvantagens, por isso é importante escolher o mais adequado para a sua situação.

## Veja também:
- [Documentação oficial do PHP sobre autenticação básica em requisições HTTP](https://www.php.net/manual/pt_BR/features.http-auth.php)
- [Artigo sobre segurança em autenticação HTTP](https://www.padillaio.com/seguranca/http-basic-authentication/)