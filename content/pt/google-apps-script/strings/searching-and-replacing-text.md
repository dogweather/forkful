---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:10.743632-07:00
description: "Como fazer: O Google Apps Script oferece uma maneira direta de buscar\
  \ e substituir texto, especialmente dentro do Google Docs e Sheets. Abaixo est\xE3\
  o\u2026"
lastmod: '2024-03-13T22:44:46.090677-06:00'
model: gpt-4-0125-preview
summary: O Google Apps Script oferece uma maneira direta de buscar e substituir texto,
  especialmente dentro do Google Docs e Sheets.
title: Pesquisando e substituindo texto
weight: 10
---

## Como fazer:
O Google Apps Script oferece uma maneira direta de buscar e substituir texto, especialmente dentro do Google Docs e Sheets. Abaixo estão exemplos para ambos.

### Google Docs:
Para buscar e substituir texto em um Documento do Google, você interagirá principalmente com a classe `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Para buscar e substituir uma frase específica
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Uso
searchReplaceInDoc();
```

Este trecho de código busca por todas as ocorrências de `'searchText'` no Documento do Google ativo e as substitui por `'replacementText'`.

### Google Sheets:
De maneira similar, no Google Sheets, você pode usar `SpreadsheetApp` para realizar operações de busca e substituição:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Buscar e substituir na planilha ativa atualmente
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Uso
searchReplaceInSheet();
```

Neste exemplo, `createTextFinder('searchText')` busca na planilha ativa por 'searchText', e `replaceAllWith('replacementText')` substitui todas as ocorrências por 'replacementText'.

## Aprofundamento
A funcionalidade de buscar e substituir no Google Apps Script é fortemente influenciada pela sua natureza baseada na web, permitindo que scripts manipulem texto em diversos aplicativos do Google de forma integrada. Historicamente, essa capacidade deriva do contexto mais amplo do processamento e manipulação de texto na programação, onde expressões regulares e funções de string em linguagens como Perl e Python estabeleceram um alto padrão para flexibilidade e potência.

Embora a funcionalidade de busca e substituição do Google Apps Script seja poderosa para substituições diretas, ela carece das capacidades completas de expressões regulares encontradas em algumas outras linguagens. Por exemplo, enquanto você pode usar expressões regulares básicas em `createTextFinder` no Google Sheets, as opções para correspondência de padrões complexos e manipulação são limitadas em comparação com Perl ou Python.

Para necessidades mais avançadas de processamento de texto, os programadores podem recorrer à exportação do conteúdo do Google Docs ou Sheets para um formato que possa ser processado externamente com linguagens mais poderosas ou empregar o Google Apps Script para chamar APIs externas ou serviços que ofereçam capacidades de manipulação de texto mais sofisticadas.

Apesar dessas limitações, para a maioria das tarefas típicas de busca e substituição dentro do ecossistema do Google Apps, o Google Apps Script oferece uma solução simples, eficiente e altamente integrável, adaptada às necessidades de automação e script dentro da suíte de ferramentas de produtividade do Google.
