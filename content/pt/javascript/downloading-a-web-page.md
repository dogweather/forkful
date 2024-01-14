---
title:                "Javascript: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Fazer o download de uma página da web pode ser necessário em diversas situações, como por exemplo para armazenar o conteúdo de um site offline, realizar análises de dados ou até mesmo para criar um programa de leitura de notícias automaticamente. Além disso, é uma habilidade importante a ser desenvolvida para aqueles que desejam se tornar desenvolvedores web proficientes.

## Como Fazer

Para fazer o download de uma página da web, podemos utilizar o JavaScript para realizar uma requisição HTTP para o servidor onde o site está hospedado. Para isso, é necessário utilizar a função `XMLHttpRequest`, que é responsável por enviar e receber dados de um servidor. Veja um exemplo abaixo:

```Javascript
var request = new XMLHttpRequest();
request.open('GET', 'www.example.com');
request.send();
```

Com isso, a página da URL especificada será baixada e armazenada na variável `request`. Para acessar o conteúdo da página, podemos utilizar a propriedade `responseText`, como mostrado abaixo:

```Javascript
console.log(request.responseText);
```

Isso irá imprimir no console o código HTML da página baixada. É importante lembrar que esse método só irá funcionar se a página estiver hospedada no mesmo domínio do código em que está sendo executado, caso contrário, pode ser necessário utilizar outras técnicas como o CORS (Cross-Origin Resource Sharing).

## Mergulhando Mais Profundo

Além da função `XMLHttpRequest`, também é possível utilizar a API `fetch()` para fazer o download de uma página da web. Essa API possui uma sintaxe mais simples e suporte a promessas, o que torna o código mais legível e fácil de gerenciar. Veja um exemplo abaixo:

```Javascript
fetch('www.example.com')
  .then(function(response) {
    return response.text();
  })
  .then(function(text) {
    console.log(text);
  });
```

Neste exemplo, o primeiro `then` converte o conteúdo baixado para texto e o segundo `then` o imprime no console. Também é possível utilizar expressões regulares para filtrar informações específicas do código HTML baixado.

## Veja Também

Aqui estão alguns links para mais informações e tutoriais sobre como fazer o download de páginas da web utilizando o JavaScript:

- [MDN - XMLHttpRequest](https://developer.mozilla.org/pt-BR/docs/Web/API/XMLHttpRequest)
- [MDN - Fetch API](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [W3Schools - XMLHttpRequest](https://www.w3schools.com/js/js_ajax_intro.asp)
- [W3Schools - Fetch](https://www.w3schools.com/js/js_api_fetch.asp)