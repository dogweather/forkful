---
title:                "Bash: Enviando um pedido http com autenticação básica"
simple_title:         "Enviando um pedido http com autenticação básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica pode ser útil para garantir que apenas usuários autorizados tenham acesso a certos recursos protegidos em um servidor. Isso é especialmente importante em situações em que informações confidenciais estão sendo transmitidas, como em uma aplicação bancária ou em um sistema de gerenciamento de dados.

## Como fazer:

Existem várias maneiras de enviar uma solicitação HTTP com autenticação básica em Bash, mas a abordagem mais comum é usando o comando `curl`. Veja um exemplo abaixo:

```Bash
curl -u username:password https://exemplo.com/recurso-protegido.html
```

Nesse exemplo, substitua "username" pelo nome de usuário e "password" pela senha da sua conta. A URL no final é o caminho para o recurso protegido que você deseja acessar. Quando você executar esse comando, o `curl` adicionará automaticamente a autenticação básica à sua solicitação.

## Explorando mais a fundo:

Quando você envia uma solicitação HTTP com autenticação básica, é importante entender como ela está estruturada. A autenticação básica segue um formato padrão de cabeçalho, com o nome de usuário e a senha codificados usando o esquema de codificação `base64`. Isso significa que, embora o usuário e a senha possam ser facilmente lidos, eles ainda estão em uma codificação não legível.

Existem também outras maneiras de adicionar autenticação básica a uma solicitação HTTP em Bash, como usando variáveis de ambiente ou um arquivo de configuração `.netrc`. É importante consultar a documentação para saber qual método é melhor para sua situação específica.

## Veja também:

- [Documentação oficial do `curl`](https://curl.haxx.se/docs/)
- [Explicação sobre autenticação básica em solicitações HTTP](https://www.httpwatch.com/httpgallery/authentication/#showExample10)
- [Outros métodos de autenticação em Bash](https://stackoverflow.com/questions/9637613/how-do-i-use-curl-to-send-a-request-with-bearer-token-authentication)