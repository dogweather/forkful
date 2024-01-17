---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Fish Shell: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Programando em Fish Shell: Como enviar uma solicitação HTTP com autenticação básica

## O que é e por que fazer?

Enviar uma solicitação HTTP com autenticação básica é um método utilizado por programadores para se comunicarem diretamente com um servidor web, fornecendo credenciais de autenticação para obter acesso a recursos protegidos. Isso permite que os desenvolvedores criem aplicativos que se conectam a APIs da web de terceiros, como serviços bancários ou mídias sociais, de forma segura e eficiente.

## Como fazer:

Usando o Fish Shell, é possível enviar uma solicitação HTTP com autenticação básica de forma simples e direta. Basta seguir esses passos:

1. Defina as variáveis necessárias: ```set username <usuário>``` e ```set password <senha>```.
2. Crie a string de autenticação com base nas variáveis definidas acima: ```set auth (base64 -d <<< "$username:$password")```.
3. Envie a solicitação HTTP com o cabeçalho de autenticação incluído: ```curl -H "Authorization: Basic $auth" <URL>```

Se a solicitação for bem-sucedida, você receberá uma resposta do servidor contendo os dados solicitados.

## Aprofundando:

A autenticação básica é um método de autenticação HTTP que existe desde o início da web e é considerado menos seguro em comparação com outros métodos, como OAuth. No entanto, é amplamente suportado pelos servidores web e ainda é usado em muitos casos.

Existem outras ferramentas disponíveis para enviar solicitações HTTP com autenticação básica, como Postman e cURL. No entanto, o Fish Shell fornece uma maneira rápida e eficiente de fazer isso diretamente na linha de comando.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Documentação do comando ```curl```](https://curl.haxx.se/docs/manpage.html)
- [Mais informações sobre autenticação básica](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#Autentica%C3%A7%C3%A3o_b%C3%A1sica)