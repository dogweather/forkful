---
title:                "Baixando uma página da web"
html_title:           "TypeScript: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é isso e por que fazemos isso?

Baixar uma página da web é quando um programador usa código para obter o conteúdo de uma página da web e salvá-lo em seu computador. Fazemos isso para ter acesso offline ao conteúdo de uma página, bem como para analisar e manipular os dados para nossos projetos.

 ## Como fazer:

 ```TypeScript
 import request from 'request';

 request('https://www.example.com', (error, response, body) => {
    if (!error && response.statusCode == 200) {
        console.log(body); // O conteúdo da página será exibido no console
    }
 });
 ```
Exemplo de saída:

```
<html>
  <head>
    <title>Exemplo</title>
  </head>
  <body>
      <h1>Bem-vindo ao exemplo</h1>
      <p>Este é um exemplo de uma página baixada usando TypeScript.</p>
  </body>
</html>
```

## Profundidade:

Baixar páginas da web tem sido uma prática comum entre os programadores desde o início da internet. Antes do TypeScript, essa tarefa era feita principalmente em outras linguagens de programação, como Python e Java. Hoje, o TypeScript oferece uma abordagem mais moderna e eficiente para lidar com a tarefa de download de páginas da web.

Além do pacote "request" utilizado no exemplo acima, existem outras opções para download de páginas da web em TypeScript, incluindo pacotes específicos para diferentes tarefas, como extração de dados ou automação de navegação.

## Veja também:

- [Documentação do TypeScript](https://www.typescriptlang.org/)
- [Documentação do pacote "request"](https://www.npmjs.com/package/request)
- [Outra opção para download de páginas da web: "node-fetch"](https://www.npmjs.com/package/node-fetch)