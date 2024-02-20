---
date: 2024-01-20 18:02:55.001975-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica significa\
  \ incluir as credenciais do usu\xE1rio (usu\xE1rio e senha) na requisi\xE7\xE3o\
  \ para acessar recursos\u2026"
lastmod: 2024-02-19 22:05:05.357159
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica significa\
  \ incluir as credenciais do usu\xE1rio (usu\xE1rio e senha) na requisi\xE7\xE3o\
  \ para acessar recursos\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Que É & Porquê?

Enviar uma requisição HTTP com autenticação básica significa incluir as credenciais do usuário (usuário e senha) na requisição para acessar recursos protegidos. Programadores fazem isso para interagir com APIs ou serviços web que requerem autenticação para garantir que o acesso seja permitido apenas a usuários autorizados.

## Como Fazer:

Antes de mais nada, instale uma biblioteca de cliente HTTP, como o Axios, com `npm install axios`. Depois, use o seguinte código TypeScript para fazer uma requisição com auth básica:

```TypeScript
import axios from 'axios';

async function fetchWithBasicAuth(url: string, username: string, password: string) {
  try {
    const response = await axios.get(url, {
      auth: {
        username,
        password,
      },
    });
    console.log('Dados:', response.data);
  } catch (error) {
    console.error('Erro na requisição:', error);
  }
}

// Substitua com a URL do seu recurso protegido, e as credenciais correspondentes
fetchWithBasicAuth('https://api.exemplo.com/dados', 'meuUsuario', 'minhaSenha');
```

Saída de exemplo (depende do recurso acessado):

```
Dados: { "resultado": "Informação protegida acessada com sucesso!" }
```

## Mergulho Profundo

A autenticação básica HTTP é um método antigo e simples para enviar credenciais. Elas são codificadas em Base64 (mas não criptografadas!) e passadas no cabeçalho da requisição, o que é inseguro se não estiver sobre HTTPS. Como uma alternativa mais segura, muitos migraram para tokens mais robustos, como Bearer tokens em conjunto com OAuth. Um detalhe importante na implementação é lembrar-se de nunca expor as credenciais no cliente; elas devem ser armazenadas com segurança e usadas apenas no servidor. No TypeScript, é essencial também tipar corretamente as respostas e erros para um melhor manejo e manutenção do código.

## Veja Também

- Documentação do Axios: https://axios-http.com/docs/intro
- Autenticação básica na MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Base64 Encoding explained: https://www.base64encode.org/
- Mais sobre segurança com HTTPS: https://letsencrypt.org/getting-started/
- Introdução a OAuth 2: https://oauth.net/2/
