---
title:                "PHP: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que?

Enviar uma solicitação HTTP é uma parte essencial do desenvolvimento PHP para interagir com outras aplicações ou serviços na web. Isso permite que você obtenha dados de outros servidores ou envie informações através de formulários em seu próprio site.

## Como fazer

Para enviar uma solicitação HTTP em PHP, usaremos a função `file_get_contents()` junto com o parâmetro `http`, seguido pela URL desejada. Também podemos usar a função `file_put_contents()` para enviar informações através de um formulário usando o método POST. Veja os exemplos abaixo:

```
<?php

// Exemplo de solicitação GET
$response = file_get_contents('http://exemplo.com/api/dados');

echo $response; // Saída: { "nome": "João", "idade": 30 }
```

```
<?php

// Exemplo de envio de dados via POST
$data = ['nome' => 'Maria', 'idade' => 25];

$options = [
    'http' => [
        'method' => 'POST',
        'header' => 'Content-type: application/x-www-form-urlencoded',
        'content' => http_build_query($data)
    ]
];

$context = stream_context_create($options);

$response = file_get_contents('http://exemplo.com/api/registro', false, $context);

echo $response; // Saída: Registro realizado com sucesso!
```

## Mergulho profundo (Deep Dive)

O PHP oferece uma série de ferramentas para lidar com solicitações HTTP, como as funções `file_get_contents()` e `file_put_contents()` mencionadas anteriormente, além das funções `fopen()` e `fwrite()` para comunicação através de sockets. Também é possível usar a biblioteca cURL para realizar solicitações mais complexas, como autenticação e envio de cabeçalhos personalizados.

Vale ressaltar que, ao enviar uma solicitação HTTP em PHP, devemos sempre garantir que estamos lidando com dados confiáveis e devidamente validados para evitar possíveis vulnerabilidades de segurança em nosso código.

## Veja também

- [Documentação oficial do PHP sobre solicitações HTTP](https://www.php.net/manual/pt_BR/book.http.php)
- [Tutorial completo sobre como enviar solicitações HTTP em PHP](https://www.php.net/manual/pt_BR/book.http.php)
- [Tutorial sobre autenticando e personalizando solicitações com cURL em PHP](https://www.php.net/manual/pt_BR/book.http.php)