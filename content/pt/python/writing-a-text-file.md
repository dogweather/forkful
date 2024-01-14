---
title:                "Python: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto

Escrever um arquivo de texto é uma tarefa comum em programação, especialmente em Python. Isso permite que você armazene informações em um formato legível e facilmente acessível. Isso pode ser útil para armazenar dados, gerar relatórios ou até mesmo criar um registro de eventos. Aprender como escrever um arquivo de texto em Python é essencial para qualquer programador.

## Como escrever um arquivo de texto em Python

Para escrever um arquivo de texto em Python, primeiro precisamos abrir o arquivo em um modo de gravação, usando o comando `open()` e passando o nome do arquivo e o modo como parâmetros. Em seguida, podemos usar o método `write()` para adicionar o conteúdo do arquivo. Por fim, precisamos fechar o arquivo usando o método `close()` para garantir que todas as alterações sejam salvas.

Um exemplo de código para escrever "Hello World!" em um arquivo de texto chamado `texto.txt` seria:

```Python
arquivo = open("texto.txt", "w")
arquivo.write("Hello World!")
arquivo.close()
```

Isso criaria um arquivo chamado `texto.txt` com o conteúdo "Hello World!". Se quisermos adicionar mais conteúdo ao arquivo, podemos usar o modo de anexação `a` ao abrir o arquivo.

```Python
arquivo = open("texto.txt", "a")
arquivo.write("Adicionando mais conteúdo.")
arquivo.close()
```

## Profundidade na escrita de arquivos de texto

Além da escrita básica de arquivos de texto, existem algumas coisas a se considerar para garantir que seu código seja eficiente e seguro. Por exemplo, é importante sempre fechar o arquivo usando o método `close()`, caso contrário, as alterações podem não ser salvas. Também é recomendável usar o bloco `with` ao abrir um arquivo, pois isso garante que o arquivo será fechado automaticamente, mesmo em caso de erros.

Outra consideração importante é a codificação do arquivo. Em alguns casos, pode ser necessário especificar a codificação ao abrir o arquivo, especialmente se ele contiver caracteres especiais ou acentos. Isso pode ser feito passando o parâmetro `encoding` ao abrir o arquivo.

```Python
arquivo = open("arquivo.txt", "w", encoding="utf-8")
```

Por fim, é importante lidar com possíveis erros ao escrever em um arquivo de texto. Isso pode ser feito usando o bloco `try/except` para capturar exceções e lidar com elas adequadamente.

## Ver também

- [Documentação oficial do Python para a função `open()`](https://docs.python.org/3/library/functions.html#open)
- [Post do DevMedia: Como ler e escrever arquivos em Python](https://www.devmedia.com.br/como-ler-e-escrever-arquivos-em-python/40366)
- [Tutorial do Real Python: Manipulando arquivos de texto em Python](https://realpython.com/read-write-files-python/)