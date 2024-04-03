---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:35.159926-07:00
description: "Como fazer: No Google Apps Script, o servi\xE7o `UrlFetchApp` \xE9 fundamental\
  \ para baixar conte\xFAdo da web. Abaixo est\xE1 um guia passo a passo e um exemplo\u2026"
lastmod: '2024-03-13T22:44:46.106037-06:00'
model: gpt-4-0125-preview
summary: "No Google Apps Script, o servi\xE7o `UrlFetchApp` \xE9 fundamental para\
  \ baixar conte\xFAdo da web."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## Como fazer:
No Google Apps Script, o serviço `UrlFetchApp` é fundamental para baixar conteúdo da web. Abaixo está um guia passo a passo e um exemplo simples demonstrando como buscar e registrar o conteúdo HTML de uma página web:

1. **Operação Básica de Busca:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Este código busca o conteúdo HTML de example.com e o registra. É uma demonstração direta de como obter a fonte de uma página web sem quaisquer parâmetros adicionais.

2. **Tratamento de Redirecionamentos e HTTPS:**

Para HTTPS ou tratamento de redirecionamentos, o código permanece em grande parte o mesmo, mas considere implementar tratamento de erros ou opções específicas para redirecionamentos:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Seguir redirecionamentos automaticamente
    'muteHttpExceptions': true // Silenciar possíveis exceções para tratá-las de forma adequada
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Limites de Frequência e Cotas:**

Esteja ciente das cotas do Google Apps Script; um uso intensivo pode requerer tratamento de erros para limites de frequência.

## Aprofundamento
Historicamente, o download e a manipulação de conteúdo web começaram com simples solicitações HTTP, evoluindo significativamente com o advento de linguagens de script. O Google Apps Script permite a execução direta de tais tarefas dentro do ecossistema G Suite, aproveitando a robusta infraestrutura do Google. O serviço `UrlFetchApp` é um elemento central dessa funcionalidade, encapsulando solicitações HTTP/S complexas em uma interface de aplicativo mais simples.

Apesar de sua conveniência, o Google Apps Script pode não ser sempre a melhor ferramenta para web scraping pesado ou quando o pós-processamento complexo dos dados buscados é necessário devido aos limites de tempo de execução e cotas impostas pelo Google. Nestes casos, frameworks de web scraping dedicados ou linguagens projetadas para operações de E/S assíncronas, como Node.js com bibliotecas como Puppeteer ou Cheerio, podem oferecer mais flexibilidade e poder.

Além disso, embora o Google Apps Script seja uma excelente ferramenta para integrar com os Serviços do Google (como Sheets, Docs e Drive) e realizar operações leves de busca de dados, é crucial ter em mente as limitações do ambiente de execução. Para tarefas intensivas, considere usar Google Cloud Functions ou os serviços avançados do Apps Script com recursos de computação externos para processamento.
