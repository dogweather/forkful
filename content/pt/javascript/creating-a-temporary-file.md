---
title:                "Criando um arquivo temporário"
html_title:           "Javascript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Existem diversas razões pelas quais alguém pode precisar criar um arquivo temporário em um programa JavaScript. Alguns motivos comuns são armazenar dados temporariamente para usar em uma sessão específica, gerenciar arquivos temporários para criar backups ou otimizar o desempenho do programa ao evitar manter dados desnecessários na memória por muito tempo.

## Como Fazer

Criar um arquivo temporário em JavaScript pode ser feito de diferentes maneiras, dependendo da plataforma e do ambiente em que você está trabalhando. Abaixo estão alguns exemplos de como criar um arquivo temporário usando Node.js e em um navegador web.

### Utilizando Node.js

Em um ambiente Node.js, você pode criar um arquivo temporário usando o módulo "fs" (file system). Primeiro, é necessário importar o módulo usando require():

```Javascript
const fs = require('fs');
```

Em seguida, podemos usar a função writeFile() para criar um arquivo temporário e escrever dados nele. Essa função recebe três argumentos: o nome do arquivo, os dados que serão gravados e uma função de callback.

```Javascript
fs.writeFile('meuArquivoTemp.txt', 'Este é um arquivo temporário!', function(err) {
  if (err) throw err;
  console.log('Arquivo temporário criado com sucesso!');
});
```

Podemos também usar a função unlink() para apagar o arquivo temporário depois de usá-lo. Esta função recebe o nome do arquivo e uma função de callback.

```Javascript
fs.unlink('meuArquivoTemp.txt', function(err) {
  if (err) throw err;
  console.log('Arquivo temporário apagado com sucesso!');
});
```

### Utilizando um navegador web

Em um navegador web, podemos criar um arquivo temporário usando o objeto File da API de arquivos do HTML5. Primeiro, precisamos criar um objeto Blob para armazenar os dados que serão gravados no arquivo e, em seguida, utilizar a função createObjectURL() para criar um URL para o arquivo.

```Javascript
var dados = new Blob(['Este é um arquivo temporário!'], { type: 'text/plain' });
var url = window.URL.createObjectURL(dados);
```

Depois, podemos criar um link para este URL e colocá-lo em um elemento <a> para que o usuário possa baixar o arquivo.

```Javascript
var link = document.createElement('a');
link.href = url;
link.download = 'meuArquivoTemp.txt';
link.click();
```

## Deep Dive

Ao criar um arquivo temporário, é importante ter em mente que ele não deve ser usado como um meio permanente de armazenamento de dados. Estes arquivos são geralmente salvos em um local temporário e apagados após seu uso, portanto, não devemos confiar neles para armazenar dados importantes por um longo período de tempo. Além disso, é importante garantir que os arquivos temporários sejam apagados depois de seu uso para evitar o acúmulo de dados desnecessários.

## Veja Também

- [Documentação do módulo fs do Node.js](https://nodejs.org/api/fs.html)
- [Documentação da API de arquivos do HTML5](https://developer.mozilla.org/en-US/docs/Web/API/File)