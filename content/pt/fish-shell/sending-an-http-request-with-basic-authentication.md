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

## Por que?

Se você está trabalhando com APIs (interfaces de programação de aplicações), é provável que, em algum momento, precise enviar uma solicitação HTTP autenticada com autenticação básica. Isso geralmente é necessário para acessar dados protegidos ou realizar ações específicas na API.

## Como fazer

O Shell Fish possui um recurso integrado que facilita o envio de solicitações HTTP com autenticação básica. Veja como fazer isso em alguns passos simples:

1. Defina as variáveis `username` e `password` com suas credenciais de autenticação:

```Fish Shell
set username "seu_usuario"
set password "sua_senha"
```

2. Crie uma string com o cabeçalho de autenticação, utilizando a função `printf` para formatar as variáveis criadas anteriormente:

```Fish Shell
set auth_header (printf "Authorization: Basic %s" (base64 -w0 (printf "%s:%s" $username $password)))
```

3. Envie a solicitação HTTP utilizando o comando `curl` e passando a opção `-H` para adicionar o cabeçalho de autenticação à solicitação:

```Fish Shell
curl -H $auth_header https://api.com/recurso/protegido
```

4. Você também pode adicionar outras opções, como `-X` para definir o método da solicitação ou `-d` para enviar dados no corpo da solicitação, conforme necessário.

## Mergulho profundo

Ao enviar uma solicitação HTTP com autenticação básica, é importante entender como esse tipo de autenticação funciona. Basicamente, as credenciais de autenticação são convertidas para Base64 e adicionadas ao cabeçalho da solicitação utilizando o formato `Authorization: Basic <credenciais>`, onde `<credenciais>` é a string codificada em Base64 das credenciais no formato `usuário:senha`.

É importante mencionar que a autenticação básica não é considerada segura, pois as credenciais são enviadas em texto simples. É sempre uma boa ideia garantir que a conexão seja feita através de HTTPS.

## Veja também

- Documentação oficial do comando `curl` em inglês: https://curl.haxx.se/docs/
- Tutorial completo sobre como enviar solicitações HTTP com autenticação básica utilizando o Shell Fish: <ins>link para tutorial em português</ins>