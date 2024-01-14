---
title:    "Javascript: Lendo um arquivo de texto"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto é importante

Se você é um programador Javascript, ler e manipular arquivos de texto é uma habilidade que você definitivamente deve ter. Isso permitirá que você trabalhe com dados externos, o que é crucial em muitos projetos de desenvolvimento web. Mas mais especificamente, ler um arquivo de texto é uma tarefa fundamental para lidar com dados estruturados e armazenamento de informações.

## Como fazer a leitura de um arquivo de texto em Javascript

Para ler um arquivo de texto em Javascript, precisamos usar o objeto `FileReader` que nos permite ler e exibir o conteúdo de um arquivo. Primeiro, precisamos de um elemento HTML chamado `<input type="file">` para permitir que o usuário selecione o arquivo que desejamos ler. Em seguida, usaremos o evento `onload` para carregar o conteúdo do arquivo selecionado. Dentro desse evento, usaremos o método `readAsText()` para ler o arquivo e, finalmente, exibiremos o conteúdo usando `console.log()`.

```
<input type="file" onchange="processFile(this.files[0])">

<script>
function processFile(file) {
  let reader = new FileReader();

  reader.onload = function(e) {
    let content = e.target.result;
    console.log(content);
  };

  reader.readAsText(file);
}
</script>
```

Ao executar este código, podemos ver no console do navegador o conteúdo do arquivo de texto que selecionamos.

## Aprofundando na leitura de arquivos de texto em Javascript

Existem muitas outras maneiras de ler arquivos de texto em Javascript, incluindo o uso de bibliotecas externas como o `node.js` ou o `fs`, que oferecem mais recursos e flexibilidade na leitura e manipulação de arquivos. Além disso, é importante lembrar de sempre manipular os erros que podem ocorrer ao ler um arquivo, como arquivos inexistentes ou com permissão de leitura negada.

Com a leitura de arquivos de texto, também é possível realizar diferentes operações, como a leitura de arquivos em diferentes formatos (JSON, CSV, etc.), a escrita de dados em um arquivo e até mesmo a extração de informações específicas de um arquivo.

Esperamos que este artigo tenha sido útil para você entender a importância e como fazer a leitura de um arquivo de texto em Javascript. Pratique e experimente diferentes métodos e bibliotecas para se familiarizar com as diferentes opções disponíveis.

## Veja também
- [Documentação oficial do FileReader](https://developer.mozilla.org/pt-BR/docs/Web/API/FileReader)
- [Manipulando arquivos com node.js](https://www.codementor.io/@moyosore/a-beginner-s-introduction-to-reading-files-in-node-js-1xmkon8xb6)
- [Manipulando arquivos de texto com fs em node.js](https://medium.com/@olayinkaraheem/reading-writnig-files-with-fswith-node-js-43f1dbd4e742)