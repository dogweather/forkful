---
title:                "Análise de HTML"
date:                  2024-01-20T15:32:35.175454-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Analisar HTML, ou "parsing", é o processo de traduzir o código HTML para uma estrutura de dados que o JavaScript possa manipular. Programadores fazem isso para interagir com o conteúdo da página, extrair informações e até mesmo modificar o DOM (Document Object Model) em tempo real.

## Como Fazer:

Vamos usar o DOMParser, que é incorporado na maioria dos navegadores:

```javascript
const parser = new DOMParser();
const htmlString = `
  <div>
    <p>Olá, mundo!</p>
  </div>
`;

const doc = parser.parseFromString(htmlString, "text/html");

console.log(doc.body.firstChild); // Acessa <div> com <p> dentro
console.log(doc.querySelector('p').textContent); // Mostra "Olá, mundo!"
```

Com isso, o conteúdo HTML é convertido para um objeto `Document`, permitindo que você manipule como quiser.

## Aprofundamento:

O DOMParser é suportado na maioria dos navegadores modernos e representa uma maneira segura e padronizada de analisar HTML. Alternativamente, havia métodos menos seguros e não recomendados, como o `innerHTML`, que também atualiza o DOM mas pode ser vulnerável a ataques de injeção de scripts (XSS).

No passado, bibliotecas como jQuery eram comuns para abstrair as complexidades do DOM, mas com o avanço dos padrões web, essas necessidades diminuíram. Para analisar HTML no lado do servidor ou em ambientes que não sejam de navegadores, você pode usar bibliotecas como o `node-html-parser` ou `cheerio` no Node.js.

Implementações de parsers HTML variam em complexidade. Eles precisam considerar especificações do HTML e lidar com marcadores mal-formados, uma realidade comum na web.

## Veja Também:

- MDN Web Docs sobre DOMParser: https://developer.mozilla.org/pt-BR/docs/Web/API/DOMParser
- Documentação do `node-html-parser`: https://github.com/taoqf/node-html-parser
- Documentação do Cheerio: https://cheerio.js.org/
