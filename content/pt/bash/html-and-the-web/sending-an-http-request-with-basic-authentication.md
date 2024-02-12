---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:00:49.342662-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Enviar uma requisição HTTP com autenticação básica é um método para acessar recursos protegidos na web usando nome de usuário e senha. Programadores usam isso para interagir com APIs ou serviços web que necessitam de credenciais para acesso.

## Como Fazer:

Para um rápido giro pelo assunto, eis um exemplo usando o `curl`:

```Bash
# Envia uma requisição GET com autenticação básica
curl -u usuario:senha http://exemplo.com/recurso

# Saída esperada: detalhes do recurso solicitado ou mensagem de erro
```

Se você prefere não colocar a senha diretamente na linha de comando, `curl` pode pedir interativamente:

```Bash
# -u com o nome de usuário apenas, `curl` pedirá a senha
curl -u usuario http://exemplo.com/recurso

# Digite a senha quando for solicitada e veja a saída.
```

Também dá para codificar a senha em base64 e usar diretamente nos cabeçalhos da requisição:

```Bash
# Codifica as credenciais em base64
credenciais=$(echo -n usuario:senha | base64)

# Envia a requisição com o cabeçalho de Autorização
curl -H "Authorization: Basic $credenciais" http://exemplo.com/recurso

# Saída esperada: detalhes do recurso
```

## Aprofundando

A autenticação básica em HTTP, uma das maneiras mais simples de controlar o acesso a recursos na web, existe desde os primórdios da internet. Usuários e senhas são codificados em base64 e enviados no cabeçalho da requisição. Embora simples, é considerada insegura se não usada com HTTPS, pois os dados podem ser facilmente decodificados se interceptados.

Alternativas mais seguras incluem autenticação Digest, OAuth e tokens de acesso. Estes métodos oferecem mais segurança, mas são mais complexos para implementar.

Detalhes da implementação de autenticação básica em Bash geralmente giram em torno do `curl`, devido à sua simples sintaxe e vasta adoção. No entanto, scripts Bash podem também aproveitar outras ferramentas como `wget` ou utilizar recursos internos do próprio Bash para criar sockets e enviar requisições diretamente, embora seja bem mais complicado.

## Veja Também

- Documentação oficial do `curl`: https://curl.se/docs/manpage.html
- Guia de autenticação HTTP da Mozilla Developer Network (MDN): https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Tutorial sobre segurança em APIs: https://auth0.com/blog/which-is-the-best-api-authentication-method/
