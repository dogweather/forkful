---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Analisar HTML (parsing HTML) é o processo de analisar o código HTML para extrair informações estruturais, como atributos, tags e metadados. Este processo é útil para automatizar a extração de dados, testar a consistência da estrutura da página e manipular o DOM para ações dinâmicas no lado do cliente.

## Como Fazer:

O JavaScript fornece muitos métodos para analisar HTML. O seguinte exemplo mostra como analisar uma string de código HTML utilizando o DOMParser:

```Javascript
var str = '<div id="meuId">Olá Mundo!</div>';
var analisador = new DOMParser();
var doc = analisador.parseFromString(str, 'text/html');
console.log(doc.body.firstChild.id);  // imprime: meuId
console.log(doc.body.firstChild.textContent);  // imprime: Olá Mundo!
```

Se quiser usar jQuery, pode ser ainda mais simples:

```Javascript
var str = '<div id="meuId">Olá Mundo!</div>';
var el = $(str);
console.log(el.attr('id'));  // imprime: meuId
console.log(el.text());  // imprime: Olá Mundo!
```

## Mergulho Profundo

No início, a análise de HTML era um processo desajeitado, sempre se baseava em expressões regulares. Graças a melhorias no padrão DOM e à introdução da API DOMParser, os programadores têm agora uma forma elegante e mais segura de analisar o HTML. 

Não obstante, existem alternativas caso você esteja trabalhando com ambientes sem DOM. Cheerio é um exemplo, que imita a sintaxe jQuery e fornece uma análise eficiente e flexível de HTML no Node.js.

A implementação da análise de HTML depende do seu objetivo final. Em termos de desempenho, uma análise estritamente necessária será mais eficiente. Mas, por vezes, necessita de uma análise completa para manipular o DOM dinamicamente.

## Veja Também

2. [jQuery API](https://api.jquery.com/): documentação oficial para a sintaxe jQuery.
3. [Cheerio.js](https://cheerio.js.org/): excelente biblioteca para análise de HTML no ambiente Node.js.