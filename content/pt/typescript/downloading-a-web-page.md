---
title:                "TypeScript: Fazendo Download de uma Página Web"
simple_title:         "Fazendo Download de uma Página Web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Baixar uma página da web pode ser útil em várias situações, como por exemplo, criar um cache local para melhorar o desempenho da sua aplicação, ou então extrair informações específicas de um site para uso em outro contexto.

## Como fazer isso em TypeScript?

Para baixar uma página da web em TypeScript, você pode utilizar a biblioteca Node.js "node-fetch". Primeiro, certifique-se de ter o Node.js instalado em sua máquina. Em seguida, crie um novo diretório e inicialize um projeto Node.js com o comando ```npm init```.

Após isso, instale o "node-fetch" com o comando ```npm install node-fetch```. Agora, você pode importar o módulo "node-fetch" em seu código TypeScript com o comando ```import fetch from 'node-fetch'```.

Então, basta usar o método ```fetch()``` e passar como parâmetro a URL da página que deseja baixar. Você pode usar o método ```text()``` para obter os dados no formato de texto, ou então o método ```json()``` para transformar em um objeto JSON.

Exemplo de código:

```
import fetch from 'node-fetch';

async function baixarPagina() {
  const response = await fetch('https://www.meusite.com');
  const dados = await response.text();
  console.log(dados);
}

baixarPagina();
```

Exemplo de saída no terminal:

```
<!DOCTYPE html>
<html>
<head>
  <title>Meu Site</title>
</head>
<body>
  <h1>Bem-vindo ao meu site!</h1>
  <p>Aqui você encontra informações e novidades sobre Tecnologia.</p>
</body>
</html>
```

## Uma olhada mais profunda

Quando usamos o método ```fetch()```, ele retorna uma Promessa que é resolvida quando a requisição é completada e nos permite trabalhar de forma assíncrona. Além disso, podemos passar algumas opções adicionais, como headers customizados, por exemplo.

Também é possível utilizar o "request-promise" em vez do "node-fetch", caso prefira uma sintaxe mais simples e direta. Esta biblioteca também lida com a conversão automática dos dados para diferentes formatos, como JSON, texto ou buffer.

## Veja também

- [Node.js](https://nodejs.org/pt-br/)
- [Documentação do node-fetch](https://www.npmjs.com/package/node-fetch)
- [Documentação do request-promise](https://www.npmjs.com/package/request-promise)