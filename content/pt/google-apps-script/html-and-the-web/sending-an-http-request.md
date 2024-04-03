---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:46.959666-07:00
description: "Como: No Google Apps Script, a maneira prim\xE1ria de enviar uma requisi\xE7\
  \xE3o HTTP \xE9 utilizando o servi\xE7o `UrlFetchApp`. Esse servi\xE7o fornece m\xE9\
  todos para\u2026"
lastmod: '2024-03-13T22:44:46.103762-06:00'
model: gpt-4-0125-preview
summary: "No Google Apps Script, a maneira prim\xE1ria de enviar uma requisi\xE7\xE3\
  o HTTP \xE9 utilizando o servi\xE7o `UrlFetchApp`."
title: "Enviando uma solicita\xE7\xE3o HTTP"
weight: 44
---

## Como:
No Google Apps Script, a maneira primária de enviar uma requisição HTTP é utilizando o serviço `UrlFetchApp`. Esse serviço fornece métodos para fazer requisições HTTP GET e POST. Aqui está um exemplo simples de fazer uma requisição GET para recuperar dados JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Para uma requisição POST, que é comumente usada para enviar dados a um servidor, você precisa incluir mais detalhes no parâmetro de opções:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    chave1: 'valor1',
    chave2: 'valor2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Converte o objeto JavaScript para uma string JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Esses trechos mostram implementações básicas de requisições GET e POST. A saída dependerá da resposta da API e pode ser vista no Logger do Google Apps Script.

## Aprofundamento
O serviço `UrlFetchApp` do Google Apps Script evoluiu significativamente desde sua criação, oferecendo um controle mais matizado sobre as requisições HTTP com funcionalidades como definição de cabeçalhos, payload e manipulação de multipart/form-data para upload de arquivos. Embora forneça um meio direto para integrar serviços web externos, desenvolvedores vindos de linguagens de backend mais robustas podem achar sua funcionalidade um tanto limitada comparada a bibliotecas como a `requests` do Python ou a API `fetch` do JavaScript no Node.js.

Uma limitação notável é o limite de tempo de execução para o Google Apps Script, que afeta requisições de longa duração. Além disso, embora o `UrlFetchApp` cubra uma ampla gama de casos de uso, cenários mais complexos envolvendo autenticação OAuth ou manipulação de cargas muito grandes podem requerer soluções criativas ou o aproveitamento de recursos adicionais do Google Cloud.

No entanto, para a maioria das integrações que os desenvolvedores do Google Workspace encontram — variando da automação da recuperação de dados a postar atualizações em serviços externos — o `UrlFetchApp` fornece uma ferramenta potente e acessível. Sua integração no Google Apps Script significa que não há necessidade de bibliotecas externas ou configuração complexa, tornando as requisições HTTP relativamente diretas de serem executadas dentro das limitações do Google Apps Script. À medida que a paisagem das APIs web continua a se expandir, o `UrlFetchApp` permanece uma ponte crítica para os programas do Google Apps Script interagirem com o mundo além do ecossistema do Google.
