---
title:    "Javascript: Lendo um arquivo de texto."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Javascript?

Ler arquivos de texto é uma tarefa comum para programadores, especialmente em linguagens como Javascript, que têm uma variedade de funções e métodos que permitem manipular e extrair informações de arquivos de texto. Neste artigo, discutiremos a importância de ser capaz de ler arquivos de texto em Javascript e como fazê-lo.

## Como ler um arquivo de texto em Javascript

Para ler um arquivo de texto em Javascript, precisamos primeiro acessar o conteúdo do arquivo e, em seguida, manipulá-lo de acordo com nossas necessidades. Para isso, usaremos o objeto File do Javascript, que nos permite acessar e manipular arquivos.

Primeiramente, precisamos criar um objeto File, que pode ser feito usando o método `new File()`. Em seguida, podemos usar o método `readAsText()` para ler o conteúdo do arquivo como uma string.

Veja um exemplo de código abaixo:

```Javascript
const file = new File("meuarquivo.txt", "r");
const conteudo = file.readAsText();
console.log(conteudo);
```

No exemplo acima, criamos um objeto File chamado "file" que lerá o arquivo "meuarquivo.txt". Em seguida, usamos o método `readAsText()` para ler o conteúdo do arquivo e armazená-lo em uma variável chamada "conteudo". Finalmente, usamos o `console.log()` para imprimir o conteúdo do arquivo no console.

## Aprofundando na leitura de arquivos de texto em Javascript

Para manipular arquivos de texto em Javascript, existem diversas funções e métodos disponíveis, como `readAsArrayBuffer()`, `readAsDataURL()` e `readAsBinaryString()`. Cada um desses métodos tem sua própria finalidade e pode ser útil dependendo do conteúdo do arquivo que está sendo lido.

Além disso, é importante mencionar que a leitura de arquivos em Javascript só é possível em navegadores modernos que suportam a API File, como Google Chrome e Firefox. Essas funções não são suportadas em Internet Explorer e outros navegadores mais antigos.

## Veja também

- [Documentação oficial do Javascript para leitura de arquivos](https://developer.mozilla.org/pt-BR/docs/Web/API/File)
- [Exemplos práticos de leitura de arquivos em Javascript](https://www.w3schools.com/jsref/met_file_readastext.asp)
- [Tutorial de leitura de arquivos em Javascript](https://www.digitalocean.com/community/tutorials/reading-files-with-node-js)