---
title:                "Enviando uma solicitação http"
html_title:           "PHP: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por que enviar uma solicitação HTTP?

Enviar uma solicitação HTTP é basicamente enviar uma mensagem para um servidor web, solicitando informações ou ação. Programadores frequentemente fazem isso para acessar dados de uma API ou atualizar um banco de dados com informações do usuário.

## Como fazer:

```php
// Usando a função interna do PHP "file_get_contents" para enviar uma solicitação GET para uma URL
$response = file_get_contents('https://www.exemplo.com/api/usuarios');

// Decodificando a resposta JSON em uma matriz PHP
$usuarios = json_decode($response);

// Imprimindo o nome e e-mail do primeiro usuário na matriz
echo $usuarios[0]['nome'];
echo $usuarios[0]['email'];
```

## Profundando:

Historicamente, as solicitações HTTP eram feitas usando a biblioteca "cURL" ou a função "fopen". No entanto, com as melhorias no PHP, a função interna "file_get_contents" é uma alternativa mais fácil de usar. Além disso, existem bibliotecas de terceiros, como o Guzzle, que fornecem recursos adicionais para enviar solicitações HTTP de maneira mais eficiente e robusta.

## Veja também:

- [Documentação oficial do PHP para a função file_get_contents](https://www.php.net/manual/pt_BR/function.file-get-contents.php)
- [Documentação do Guzzle](http://docs.guzzlephp.org/en/stable/)
- [Artigo do W3 Schools sobre como enviar solicitações HTTP com o PHP](https://www.w3schools.com/Php/php_http.asp)