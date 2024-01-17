---
title:                "Analisando html"
html_title:           "Javascript: Analisando html"
simple_title:         "Analisando html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## O que & por quê?
Parcing HTML é o processo de analisar um documento HTML e extrair informações específicas de código. Programadores fazem isso para obter dados valiosos de páginas da web e usá-los para suas próprias tarefas de programação.

## Como fazer:
Para parcer HTML em Javascript, você pode usar a API nativa do navegador `DOMParser()`. Aqui está um exemplo de como usá-la:

```Javascript
// Criando uma nova instância do DOMParser
let parser = new DOMParser();

// Criando uma string contendo código HTML
let htmlString = "<div>Exemplo de HTML</div>";

// Usando o método `parseFromString()` para analisar o código HTML
let parsedHTML = parser.parseFromString(htmlString, "text/html");

// Acessando o conteúdo dentro da tag <div> analisada
console.log(parsedHTML.querySelector("div").textContent); // Saída: Exemplo de HTML
```

## Profundando:
A parceria HTML é uma técnica amplamente usada hoje em dia, mas nem sempre foi assim. No passado, os programadores tinham que escrever seu próprio código para analisar o HTML manualmente, o que era demorado e propenso a erros. Além disso, existem bibliotecas de terceiros disponíveis para parceria HTML, como o popular Cheerio.

Para uma implementação mais avançada, você pode usar a API `querySelector()` do DOM para selecionar elementos específicos do HTML usando seletores CSS. Isso pode ser útil para extrair dados de páginas da web, como informações de produtos ou de redes sociais.

## Veja também:
- [Documentação DOMParser() MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/DOMParser)
- [Cheerio Library](https://cheerio.js.org/)