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

## Por que

Você pode precisar enviar uma solicitação HTTP com autenticação básica para acessar um serviço ou API que requer autenticação apropriada.

## Como Fazer

```Bash
# Importar o pacote Curl para enviar solicitações HTTP
apt-get install curl

# Enviar uma solicitação GET com autenticação básica
curl -u username:password URL

# Enviar uma solicitação POST com autenticação básica e dados JSON
curl -u username:password -H "Content-Type: application/json" -d '{"key": "value"}' URL

# Verificar o código de resposta e o cabeçalho da solicitação
curl -u username:password -I URL
```

## Mergulho Profundo

Ao enviar uma solicitação HTTP com autenticação básica, é importante conhecer alguns detalhes importantes. Primeiramente, a autenticação básica codifica o nome de usuário e a senha em formato Base64 e os envia no cabeçalho da solicitação como "Autorização", o que significa que eles são facilmente decodificados por terceiros. Portanto, é importante utilizar uma conexão segura (HTTPS) ao enviar uma solicitação com autenticação básica. Além disso, o nome de usuário e a senha podem ser armazenados em variáveis de ambiente para serem utilizados em seus scripts Bash e evitar a exposição desnecessária.

## Veja também

- Documentação oficial do Curl: https://curl.haxx.se/docs/
- Tutorial básico de Curl: https://www.digitalocean.com/community/tutorials/introduction-to-curl