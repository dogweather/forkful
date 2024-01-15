---
title:                "Analisando html"
html_title:           "TypeScript: Analisando html"
simple_title:         "Analisando html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a tarefa de extrair informações específicas de um documento HTML. Talvez seja para criar um web scraping script ou para processar dados de uma página da web. Para isso, é necessário realizar a análise sintática do HTML, ou seja, transformá-lo em uma estrutura de dados que possa ser facilmente manipulada por um programa. A partir daí, é possível obter os dados desejados de forma mais eficiente.

## Como fazer

Realizar a análise sintática de um documento HTML pode parecer uma tarefa complexa, mas com o TypeScript essa tarefa se torna muito mais simples. Com a biblioteca `html-parser`, podemos facilmente converter um documento HTML em um objeto do tipo `HTMLElement` e acessar suas propriedades e filhos.

```TypeScript
import { parse } from 'html-parser';

const html = '<div><h1>Título</h1><p>Parágrafo</p></div>';
const parsed = parse(html);

console.log(parsed.tagName); // div
console.log(parsed.children[0].tagName); // h1
console.log(parsed.children[1].tagName); // p
```

Ao executar o código acima, podemos ver que o HTML foi convertido em um objeto que contém o nome da tag e seus filhos, facilitando assim o acesso e manipulação dos dados.

## Mergulho profundo

Além de converter o HTML em um objeto, a biblioteca `html-parser` também nos permite realizar consultas específicas em busca de elementos específicos. Isso é feito utilizando a função `querySelector`, que nos permite passar um seletor CSS como parâmetro.

```TypeScript
import { parse } from 'html-parser';

const html = '<ul><li>Item 1</li><li>Item 2</li></ul>';
const parsed = parse(html);

const items = parsed.querySelector('li');
console.log(items[0].innerText); // Item 1
console.log(items[1].innerText); // Item 2
```

Com essa funcionalidade, podemos facilmente extrair dados de qualquer tag ou classe que desejamos, tornando o processo de parsing ainda mais eficiente.

## Veja também

- Documentação oficial do `html-parser`: https://github.com/nullpunkt666/html-parser
- Tutorial de como usar o `html-parser` com TypeScript: https://www.valentinog.com/blog/html-parser-javascript/
- Exemplos práticos de parsing com o `html-parser`: https://medium.com/@rajaraodv/parsing-html-in-typescript-with-htmlparser2-8049413a260f