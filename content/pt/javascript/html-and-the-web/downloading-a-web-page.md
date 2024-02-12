---
title:                "Baixando uma página da web"
aliases:
- /pt/javascript/downloading-a-web-page/
date:                  2024-01-20T17:44:15.156493-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Baixar uma página web significa puxar o conteúdo de um site para seu próprio computador ou servidor. Programadores fazem isso para analisar dados, testar performance ou para simplesmente armazenar informações offline.

## Como fazer:
Para baixar uma página web com JavaScript, você pode usar a API Fetch, que é moderna e eficiente. Vamos a um exemplo básico:

```Javascript
fetch('https://exemplo.com')
  .then(response => response.text())
  .then(data => {
    console.log(data); // Aqui está o HTML da página!
  })
  .catch(error => {
    console.error('Erro ao baixar a página:', error);
  });
```

Saída de exemplo:
```
<!DOCTYPE html>
<html lang="pt">
<head>
...
</head>
<body>
...
</body>
</html>
```

## Aprofundando:

No passado, usávamos coisas como `XMLHttpRequest` para obter conteúdos da web, mas a Fetch API é mais potente e fácil de usar. Alternativas não faltam: além da Fetch API, temos bibliotecas como `axios` ou até o bom e velho `request` (agora depreciado).

Implementar um download de página é mais do que copiar e colar HTML. Pense também em headers de requisição, política de CORS (Cross-Origin Resource Sharing), e manipulação de cookies. Cada detalhe pode ser crucial, então não subestime a preparação para lidar com possíveis obstáculos.

## Veja Também:

- MDN Web Docs sobre a Fetch API: [developer.mozilla.org/en-US/docs/Web/API/Fetch_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Documentação do Axios: [github.com/axios/axios](https://github.com/axios/axios)
