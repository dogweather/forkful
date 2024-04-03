---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:10.507803-07:00
description: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica envolve\
  \ codificar um nome de usu\xE1rio e senha em um cabe\xE7alho de solicita\xE7\xE3\
  o para acessar recursos\u2026"
lastmod: '2024-03-13T22:44:46.107148-06:00'
model: gpt-4-0125-preview
summary: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica envolve\
  \ codificar um nome de usu\xE1rio e senha em um cabe\xE7alho de solicita\xE7\xE3\
  o para acessar recursos protegidos."
title: "Enviando uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## Como fazer:
No Google Apps Script, para enviar uma solicitação HTTP com autenticação básica, você utiliza o serviço `UrlFetchApp` combinado com um cabeçalho de autorização codificado em base64. Aqui está um guia passo-a-passo:

1. **Codificar Credenciais**: Primeiro, codifique seu nome de usuário e senha em base64. O Google Apps Script não possui uma função nativa de codificação base64 para strings, portanto, você usará Utilities.base64Encode para esse propósito.

```javascript
var username = 'SeuNomeDeUsuario';
var password = 'SuaSenha';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Configurar Opções de Solicitação**: Com as credenciais codificadas prontas, prepare o objeto de opções para a solicitação HTTP, incluindo o método e os cabeçalhos.

```javascript
var options = {
  method: 'get', // ou 'post', 'put', dependendo de suas necessidades
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // opções adicionais como 'muteHttpExceptions' para tratamento de erros podem ser adicionadas aqui
};
```

3. **Realizar a Solicitação**: Use o método `UrlFetchApp.fetch` com a URL de destino e o objeto de opções.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

A saída de exemplo após uma solicitação bem-sucedida variará com base na resposta da API. Para uma API baseada em JSON, você pode ver algo como:

```
{"status":"Success","data":"Dados do recurso aqui..."}
```

Certifique-se de lidar com possíveis erros HTTP, verificando o código de resposta ou usando a opção `muteHttpExceptions` para um gerenciamento de erros mais controlado.

## Aprofundamento
Enviar uma solicitação HTTP com autenticação básica tem sido um método padrão em muitas linguagens de programação para acessar recursos baseados na web que requerem autenticação. No contexto do Google Apps Script, `UrlFetchApp` fornece uma maneira direta de realizar essas solicitações HTTP, incluindo aquelas que requerem autenticação. A inclusão de credenciais básicas nos cabeçalhos de solicitação é um método simples, porém eficaz, mas vem com ressalvas de segurança, principalmente porque as credenciais são enviadas em texto simples, apenas codificadas em base64, o que pode ser facilmente decodificado se interceptado.

Para uma segurança aprimorada, alternativas como OAuth 2.0 são recomendadas, especialmente ao lidar com dados sensíveis ou operações. O Google Apps Script tem suporte integrado para OAuth 2.0 com a biblioteca `OAuth2`, simplificando o processo de autenticação contra serviços que suportam esse protocolo.

Apesar de suas limitações de segurança, a autenticação básica continua sendo amplamente utilizada para aplicações simples ou internas não expostas à internet mais ampla. É simples de implementar, pois requer apenas uma única solicitação com cabeçalhos adequadamente configurados, tornando-se uma opção atraente para integrações rápidas ou para APIs onde métodos de segurança mais elevados não estão disponíveis. No entanto, os programadores são instados a considerar as implicações de segurança e explorar alternativas mais seguras quando disponíveis.
