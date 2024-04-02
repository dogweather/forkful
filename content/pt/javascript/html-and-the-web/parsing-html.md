---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:45.330676-07:00
description: "Analisar HTML significa extrair dados de documentos HTML. Programadores\
  \ fazem isso para interagir com ou manipular conte\xFAdo da web, automatizar a extra\xE7\
  \xE3o\u2026"
lastmod: '2024-03-13T22:44:46.960485-06:00'
model: gpt-4-0125-preview
summary: "Analisar HTML significa extrair dados de documentos HTML. Programadores\
  \ fazem isso para interagir com ou manipular conte\xFAdo da web, automatizar a extra\xE7\
  \xE3o\u2026"
title: Analisando HTML
weight: 43
---

## O Que & Porquê?
Analisar HTML significa extrair dados de documentos HTML. Programadores fazem isso para interagir com ou manipular conteúdo da web, automatizar a extração de dados ou para fins de raspagem da web.

## Como fazer:
Vamos analisar HTML usando a API `DOMParser` em JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Olá, mundo!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Saída: Olá, mundo!
```

Agora, vamos pegar algo mais específico, como um elemento com uma classe:

```Javascript
const htmlString = `<div><p class="greeting">Olá, novamente!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Saída: Olá, novamente!
```

## Mergulho Profundo
Analisar HTML é tão antigo quanto a web. Inicialmente, era uma coisa dos navegadores—navegadores analisavam HTML para exibir páginas da web. Com o tempo, os programadores queriam explorar esse processo, levando a APIs como `DOMParser`.

Alternativas? Claro. Temos bibliotecas como `jQuery` e ferramentas como `BeautifulSoup` para Python. Mas o `DOMParser` nativo do JavaScript é rápido e já vem embutido, sem necessidade de bibliotecas extras.

Em termos de implementação, quando você analisa HTML com `DOMParser`, ele cria um objeto `Document`. Pense nisso como um modelo hierárquico do seu HTML. Uma vez que você o tem, você pode navegar e manipular como faria com o DOM de uma página da web normal.

Aqui está a coisa—o processo de análise pode tropeçar em HTML malformado. Os navegadores são tolerantes, mas o `DOMParser` pode não ser. Portanto, para tarefas complexas ou HTML bagunçado, bibliotecas de terceiros podem fazer um trabalho de limpeza melhor.

## Veja Também
- Documentação da MDN sobre a API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en/docs/Web/API/DOMParser)
- Capacidades de análise do jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, uma implementação rápida, flexível e enxuta do núcleo do jQuery para o servidor: [Cheerio.js](https://cheerio.js.org/)
- Para análise fora do JS: Biblioteca BeautifulSoup do Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
