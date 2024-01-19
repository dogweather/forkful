---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## O quê e Por quê?

Analisar HTML (ou parsing HTML) é o processo de transformar uma string de HTML em uma estrutura de dados que pode ser manipulada e consultada. Os programadores fazem isso para extrair informações, como metadados, textos e links, de documentos HTML.

## Como fazer:

Aqui está um exemplo simples usando a biblioteca cheerio em TypeScript para parsear HTML. 

```TypeScript
import * as cheerio from 'cheerio';

let htmlString = `<html><body><h1>Olá Mundo!</h1></body></html>`;

let $ = cheerio.load(htmlString);

let titulo = $('h1').text();

console.log(titulo); // imprime: Olá Mundo!
```

Neste código, a função `cheerio.load` cria uma função `$` que pode ser usada para consultar a estrutura de dados do documento HTML.

## Aprofundando:

Historicamente, a análise de HTML era um processo complicado e propenso a erros, pois cada navegador tinha seu próprio método de lidar com HTML malformado. No entanto, bibliotecas modernas como o Cheerio simplificaram muito essa tarefa, oferecendo uma API JQuery-like que é familiar para muitos desenvolvedores.

Existem também alternativas ao Cheerio, como o JSDOM, que oferece uma simulação de navegador mais completa e pode ser mais adequado para determinadas tarefas, como renderização de JavaScript de lado do servidor.

Em relação aos detalhes de implementação, o Cheerio em particular usa uma biblioteca chamada htmlparser2 para fazer o parsing do HTML e gerar uma árvore DOM. Em seguida, fornece funções para consultar e manipular essa árvore.

## Veja também:

Para mais detalhes e exemplos, veja a documentação oficial dessas bibliotecas:

- Cheerio: https://cheerio.js.org/
- JSDOM: https://github.com/jsdom/jsdom

E para uma visão mais profunda da análise de HTML, o padrão da WHATWG é um ótimo recurso: https://html.spec.whatwg.org/multipage/parsing.html#parsing