---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:49.504421-07:00
description: "Como: Usar express\xF5es regulares no Google Apps Script \xE9 simples,\
  \ gra\xE7as \xE0 sintaxe baseada em JavaScript. Aqui est\xE1 como voc\xEA pode incorporar\
  \ regex em seus\u2026"
lastmod: '2024-03-13T22:44:46.096305-06:00'
model: gpt-4-0125-preview
summary: "Usar express\xF5es regulares no Google Apps Script \xE9 simples, gra\xE7\
  as \xE0 sintaxe baseada em JavaScript."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como:
Usar expressões regulares no Google Apps Script é simples, graças à sintaxe baseada em JavaScript. Aqui está como você pode incorporar regex em seus scripts para tarefas comuns como busca e validação de dados.

### Pesquisando Strings
Suponha que você queira encontrar se uma string contém um padrão específico, como um endereço de email. Aqui está um exemplo simples:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Found: " + found[0]);
  } else {
    Logger.log("Nenhum email encontrado.");
  }
}

// Uso de exemplo
findEmailInText("Contate-nos pelo info@example.com.");
```

### Validação de Dados
Expressões regulares brilham na validação de dados. Abaixo está uma função que valida uma string de entrada para verificar se ela adere a uma política de senha simples (pelo menos uma letra maiúscula, uma letra minúscula e um mínimo de 8 caracteres).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Saída de exemplo
Logger.log(validatePassword("Str0ngPass")); // Saída: true
Logger.log(validatePassword("weak"));       // Saída: false
```

## Aprofundando
As expressões regulares no Google Apps Script são herdadas do JavaScript, padronizadas pela primeira vez na especificação da linguagem ECMAScript em junho de 1997. Embora poderosas, às vezes podem levar a códigos confusos e difíceis de manter, especialmente quando usadas excessivamente ou para tarefas de correspondência de padrões complexos que poderiam ser resolvidas mais eficientemente através de outros métodos de análise.

Por exemplo, embora você possa usar regex para análise de HTML ou XML em uma situação de aperto, fazer isso geralmente não é recomendado devido às estruturas aninhadas e intrincadas desses documentos. Em vez disso, ferramentas especificamente projetadas para analisar tais estruturas, como os analisadores DOM para HTML, são mais confiáveis e legíveis.

Além disso, os desenvolvedores do Google Apps Script devem estar atentos a possíveis problemas de desempenho ao usar padrões regex complexos em tarefas de manipulação de texto em grande escala, já que o processamento de regex pode ser intensivo em CPU. Nesses casos, dividir a tarefa em sub-tarefas mais simples ou usar funções de manipulação de string integradas poderia oferecer um melhor equilíbrio de desempenho e manutenção.
