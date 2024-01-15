---
title:                "Enviando uma solicitação http"
html_title:           "TypeScript: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Enviar um pedido HTTP é uma tarefa fundamental ao desenvolver aplicativos web. É a maneira mais comum de obter dados de um servidor e exibi-los no seu aplicativo.

## Como fazer

Para enviar um pedido HTTP em TypeScript, primeiro precisamos instalar a biblioteca `axios`. Em um projeto TypeScript existente, abra o terminal e execute o seguinte comando:

```
npm install axios
```

Isso adicionará a biblioteca `axios` ao seu arquivo `package.json` e instalará a mesma em seu diretório `node_modules`.

Então, em qualquer arquivo TypeScript em que você deseja enviar o pedido HTTP, importe a biblioteca `axios`:

```
import axios from 'axios';
```

Para enviar o pedido, usaremos o método `get()` da biblioteca `axios`, que nos permite especificar o URL do servidor e quaisquer parâmetros adicionais. Aqui está um exemplo de um pedido HTTP GET simples para a API de demonstração "jsonplaceholder":

```
axios.get('https://jsonplaceholder.typicode.com/posts')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

Dentro do método `then()`, temos acesso aos dados recebidos do servidor através da propriedade `data` do objeto `response`. Podemos usar esses dados para atualizar nosso aplicativo ou exibi-los na tela.

Se o pedido for bem-sucedido, a saída será semelhante a esta:

```
[
  {
    "userId": 1,
    "id": 1,
    "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
    "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit"
  },
  {
    "userId": 1,
    "id": 2,
    "title": "qui est esse",
    "body": "est rerum tempore vitae\nsequi sint nihil reprehenderit doloremque quia"
  },
  //...
]
```

Caso ocorra algum erro durante a solicitação, o método `catch()` será acionado e podemos lidar com o erro dentro dele.

## Imersão profunda

Ao enviar um pedido HTTP em TypeScript, existem algumas coisas importantes a serem lembradas:

- Certifique-se de adicionar `;` ao final de cada instrução, é uma boa prática de programação e pode evitar alguns possíveis erros.
- Use `try` e `catch` ao lidar com erros, assim você pode ter um melhor controle do que está acontecendo em seu aplicativo.
- Ao adicionar parâmetros ao pedido, certifique-se de usar a sintaxe correta, como por exemplo `?key1=value1&key2=value2`. Isso pode variar dependendo da API que você está usando, então sempre verifique a documentação da API.

Agora que você sabe como enviar um pedido HTTP em TypeScript, pode começar a usar essa habilidade em seus projetos e criar aplicativos web incríveis!

## Veja também

- [Documentação oficial do Axios](https://axios-http.com/docs/intro)
- [Artigo sobre manipulação de erros em TypeScript](https://medium.com/weekly-webtips/manipulando-erros-em-typescript-de1bb5763cfc?source=friends_link&sk=304d9c62d6fc17240c9b8c5c2b855f2f)
- [Tutorial sobre como consumir APIs em TypeScript](https://www.freecodecamp.org/news/how-to-make-api-calls-with-axios-and-react-9c5d3e1451b)