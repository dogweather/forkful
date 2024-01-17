---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Bash: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?
O envio de uma solicitação HTTP com autenticação básica é o processo de enviar informações de login (como nome de usuário e senha) junto com a solicitação para acessar um determinado recurso em um servidor web. Os programadores geralmente fazem isso para acessar recursos restritos ou para autenticar usuários em um sistema.

## Como fazer:
```
#!/bin/bash
read -p "Insira seu nome de usuário: " username 
read -sp "Insira sua senha: " password
echo ""
curl -u $username:$password https://www.exemplo.com/recurso
```

Saída:
```
{"mensagem": "Bem-vindo à sua conta!"}
```

## Aprofundando:
Autenticação básica é um método de autenticação padrão para solicitações HTTP usando um nome de usuário e senha em texto não criptografado. É baseado em desafio-resposta, onde o servidor envia um desafio para o cliente e o cliente responde com as informações de login codificadas em base64. É amplamente utilizado na web, mas não é considerado um método seguro, pois as informações de login são facilmente interceptadas por terceiros.

Existem alternativas mais seguras, como a autenticação baseada em tokens ou OAuth, que usam criptografia para garantir a segurança das informações de autenticação.

A implementação da autenticação básica em bash é feita usando o comando `curl` e a opção `-u`, que permite especificar o nome de usuário e senha no formato `username:password`.

## Veja também:
- [Documentação do comando curl](https://curl.haxx.se/docs/httpscripting.html)
- [Tutorial sobre autenticação básica](https://www.httpwatch.com/httpgallery/authentication/)