---
title:                "Python: Lendo um arquivo de texto"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma habilidade essencial para qualquer programador em Python. Arquivos de texto são usados ​​para armazenar informações importantes que precisamos acessar e manipular em nossos programas. Aprender como ler arquivos de texto nos permite criar programas mais poderosos e eficazes.

## Como ler um arquivo de texto em Python

Ler um arquivo de texto em Python é um processo simples. Podemos usar a função `open()` para abrir um arquivo e a função `read()` para ler seu conteúdo. Vamos ver um exemplo:

````Python
# Abrir o arquivo
arquivo = open("arquivo.txt", "r")

# Ler o conteúdo do arquivo
conteudo = arquivo.read()

# Imprimir o conteúdo
print(conteudo)

# Fechar o arquivo
arquivo.close()
````

O código acima irá abrir o arquivo "arquivo.txt" e armazenar seu conteúdo na variável `conteudo`. Em seguida, podemos imprimir o conteúdo na tela e, por fim, fechar o arquivo. É importante sempre fechar o arquivo após lê-lo para evitar problemas de memória.

## Aprofundando-se na leitura de arquivos de texto

Além de usar a função `read()`, também podemos utilizar outras funções para ler arquivos de texto em Python. Por exemplo, podemos usar a função `readline()` para ler uma linha específica do arquivo ou `readlines()` para ler todas as linhas e armazená-las em uma lista. Também é possível especificar o tamanho máximo do conteúdo a ser lido usando a função `read(size)`.

Outra coisa importante a ter em mente quando se trabalha com arquivos de texto é que precisamos informar o modo de abertura do arquivo. No exemplo anterior, usamos "r", que significa "read" (ler). Mas também podemos usar "w" para escrever em um arquivo, "a" para adicionar conteúdo a um arquivo existente ou "x" para criar um novo arquivo apenas se ele não existir.

## Veja também
- [Documentação oficial do Python sobre leitura de arquivos](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial sobre manipulação de arquivos de texto em Python](https://realpython.com/read-write-files-python/)
- [Exemplos práticos de leitura de arquivo em Python](https://www.programiz.com/python-programming/file-operation)

A leitura de arquivos de texto é uma habilidade essencial para um programador em Python. Espero que este artigo tenha lhe dado uma compreensão básica de como ler arquivos em Python e o inspire a aprofundar ainda mais seus conhecimentos. Boa sorte em sua jornada de programação em Python!