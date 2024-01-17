---
title:                "Baixando uma página da web"
html_title:           "Javascript: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Baixar uma página da web significa recuperar seu conteúdo por meio de um programa de computador. Isso é útil para desenvolvedores porque permite que eles acessem e manipulem o conteúdo de uma página da web diretamente em seu código.

## Como fazer:

```Javascript
const pagina = "https://www.exemplo.com/";
fetch(pagina)
  .then(response => response.text())
  .then(data => console.log(data));
```

Saída de exemplo:
```
<!DOCTYPE html>
<html>
<head>
  <title>Exemplo</title>
</head>
<body>
  <h1> Bem-vindo ao Exemplo </h1>
  <p> Esta é uma página de exemplo </p>
</body>
</html>
```

## Profundando:

Download de páginas da web é uma funcionalidade importante para muitos aplicativos da web. Antigamente, isso era feito com linguagens de programação como Perl ou Python. Com o aumento da popularidade de Javascript, tornou-se mais comum o uso de bibliotecas e frameworks como jQuery ou Axios para realizar essa tarefa. Além disso, o elemento `fetch` é uma adição recente ao padrão Javascript que torna mais fácil e direta a recuperação de conteúdo de páginas da web.

## Veja também:

- [Learn Javascript: Making a Request to a Server](https://www.learn-js.org/en/Fetching_Data)
- [MDN Web Docs: Fetch API](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [jQuery: API Documentation](https://api.jquery.com/)