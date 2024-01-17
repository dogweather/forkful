---
title:                "Escrevendo um arquivo de texto"
html_title:           "Python: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever um arquivo de texto é simplesmente criar um documento que contém texto. Programadores fazem isso para armazenar informações, como configurações, dados de usuário ou até mesmo código.

## Como fazer:

Para escrever um arquivo de texto em Python, usamos a função `open()` que recebe dois argumentos: o nome do arquivo que queremos criar e o modo de abertura, que no nosso caso é 'w' para escrita. Em seguida, usamos o método `write()` para adicionar o conteúdo que queremos ao arquivo e, por fim, fechamos o arquivo usando o método `close()`.

```Python
arquivo = open('exemplo.txt', 'w')
arquivo.write('Este é um exemplo de arquivo de texto.')
arquivo.close()
```
O código acima cria um arquivo chamado "exemplo.txt" e escreve a frase "Este é um exemplo de arquivo de texto." nele. Você pode verificar o conteúdo do arquivo abrindo-o com um editor de texto.

## Mergulho profundo:

Escrever arquivos de texto é uma tarefa básica em qualquer linguagem de programação. Ela foi inspirada nas antigas máquinas de escrever, onde os caracteres eram impressos em papel. Nas versões mais recentes do Python, podemos usar o gerenciador de contexto `with` para abrir e fechar automaticamente o arquivo, assim não precisamos nos preocupar com o uso do método `close()`.

Também podemos usar o modo 'a' para anexar conteúdo a um arquivo existente ou o modo 'r+' para ler e escrever no mesmo arquivo.

## Veja também:

Para mais informações sobre como escrever arquivos de texto em Python, consulte a documentação oficial: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files

Você também pode conferir estes tutoriais para entender melhor como funciona a função `open()` e os modos de abertura: 
- https://www.geeksforgeeks.org/python-handling-filenames-with-special-characters-in-os-filemethods/
- https://realpython.com/read-write-files-python/