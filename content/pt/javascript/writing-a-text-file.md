---
title:    "Javascript: Escrevendo um arquivo de texto"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que

Escrever arquivos de texto pode parecer uma tarefa simples e básica para muitos programadores, mas na verdade é uma habilidade essencial em qualquer linguagem de programação. Ao aprender a escrever arquivos de texto em Javascript, você pode armazenar dados gerados por seus programas e acessá-los posteriormente, tornando seus projetos mais dinâmicos e completos.

## Como fazer

Há duas maneiras de escrever um arquivo de texto em Javascript: usando o método `writeFile` do Node.js ou criando um objeto `fileWriter` no navegador. Ambas as opções exigem a importação do módulo `fs` do Node.js. Veja abaixo um exemplo de como escrever um arquivo de texto com o método `writeFile`:

```Javascript
const fs = require('fs');

// criando um arquivo chamado "texto.txt" e escrevendo a string "Olá mundo" nele
fs.writeFile('texto.txt', 'Olá mundo', (err) => {
  if (err) throw err;
  console.log('Arquivo criado com sucesso.');
});
```
O código acima irá criar um arquivo chamado "texto.txt" no mesmo diretório do arquivo que está sendo executado e escrever a string "Olá mundo" dentro dele. Você pode verificar o arquivo criado abrindo-o em um editor de texto ou executando outro código para ler seu conteúdo.

Já para criar um objeto `fileWriter` no navegador, você pode usar o seguinte código:

```Javascript
// criando o objeto fileWriter
var writer = new FileWriter("texto.txt");

// escrevendo a string "Olá mundo" no arquivo
writer.writelinie("Olá mundo");

// fechando o objeto writer para salvar as mudanças no arquivo
writer.close();
```

Lembre-se de que, ao utilizar o objeto `fileWriter`, você também precisa garantir que o navegador tenha permissão para acessar o diretório onde o arquivo será criado.

## Mergulhando mais fundo

Agora que você já sabe como escrever um arquivo de texto em Javascript, é importante entender como trabalhar com diferentes tipos de dados. Por exemplo, se você quiser escrever um objeto em um arquivo de texto, precisará convertê-lo para uma string usando o método `JSON.stringify()` antes de gravá-lo no arquivo.

Além disso, é crucial entender como lidar com possíveis erros ao escrever um arquivo de texto. É sempre recomendável incluir uma declaração `try-catch` ao usar o método `writeFile` ou `createWriter` para garantir que seu código possa lidar com qualquer problema que possa surgir.

## Veja também

- [Documentação do Node.js sobre o módulo `fs`](https://nodejs.org/api/fs.html)
- [Tutorial de como escrever arquivos de texto em Javascript](https://flaviocopes.com/javascript-write-file/)
- [Guia completo sobre como lidar com arquivos em Javascript](https://codeburst.io/how-to-handle-files-with-javascript-f3c5b2f8bd4f)