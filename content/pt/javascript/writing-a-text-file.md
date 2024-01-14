---
title:                "Javascript: Escrevendo um arquivo de texto"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever e manipular arquivos de texto é uma habilidade fundamental na programação Javascript. Isso permite que os desenvolvedores armazenem e leiam dados de maneira fácil e eficiente. Se você deseja criar aplicativos robustos e dinâmicos, saber como escrever um arquivo de texto é essencial.

## Como fazer:

Para escrever um arquivo de texto em Javascript, você precisará utilizar o node.js. Siga os passos abaixo para criar um arquivo de texto simples contendo uma frase de exemplo:

1. Crie um novo diretório em sua máquina.
2. Abra o prompt de comando e navegue até o diretório que você criou.
3. Digite o comando `npm init` para inicializar o node.js. Siga as instruções para criar um package.json.
4. No mesmo diretório, crie um arquivo javascript com o nome "escrever-arquivo.js".
5. No arquivo, adicione o seguinte código:

```Javascript
const fs = require('fs');
const frase = "Olá, mundo! Este é um arquivo de texto criado com Javascript.";

fs.writeFile('meu-arquivo.txt', frase, (err) => {
    if (err) throw err;
    console.log("Arquivo criado com sucesso!");
});
```

6. Salve o arquivo e execute o comando `node escrever-arquivo.js` no diretório do arquivo.
7. Agora, verifique o diretório e você encontrará um novo arquivo de texto chamado "meu-arquivo.txt" contendo a frase que você definiu.

Parabéns! Você acabou de escrever um arquivo de texto usando Javascript.

## Profundidade:

Agora que você aprendeu a escrever um arquivo de texto simples, vamos analisar um exemplo mais complexo. Digamos que você queira criar um arquivo de texto com uma lista de tarefas, onde cada tarefa é uma linha no arquivo. Para fazer isso, você precisará fazer algumas alterações no código anterior:

```Javascript
const fs = require('fs');
const tarefas = ["Comprar leite", "Fazer exercícios", "Ler um livro"];

fs.writeFile('lista-de-tarefas.txt', tarefas.join('\n'), (err) => {
    if (err) throw err;
    console.log("Arquivo criado com sucesso!");
});
```

Neste exemplo, usamos o método `join()` para transformar o array de tarefas em uma única string com quebras de linha entre cada item. Quando o arquivo é criado, cada tarefa estará em uma linha separada.

## Veja também:

- [Manipulando arquivos com node.js](https://nodejs.org/api/fs.html)
- [Tutorial de node.js para iniciantes](https://www.w3schools.com/nodejs/)
- [Exemplos de coding com Javascript](https://www.javascript.com/learn)

Agora que você aprendeu a escrever um arquivo de texto em Javascript, experimente diferentes tipos de dados e explore as diferentes opções do objeto `fs`. Continue praticando e aprimorando suas habilidades de programação com Javascript. Boa sorte!