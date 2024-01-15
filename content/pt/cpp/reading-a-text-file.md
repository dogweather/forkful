---
title:                "Lendo um arquivo de texto"
html_title:           "C++: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler um arquivo de texto pode ser essencial em vários cenários de programação. Isso pode incluir a manipulação de dados externos, criação de relatórios ou até mesmo para implementar funcionalidades de leitura e escrita em um programa.

## Como fazer?

A leitura de um arquivo de texto em C++ pode ser feita utilizando a biblioteca padrão "fstream". Primeiro, é necessário incluir a biblioteca em seu código:
```C++
#include <fstream>
```
Em seguida, é preciso criar um objeto do tipo "fstream" e abrir o arquivo de texto desejado utilizando o método "open()":
```C++
std::fstream arquivo;
arquivo.open("exemplo.txt");
```
Com o arquivo aberto, podemos utilizar o método "getline()" para ler cada linha do texto e armazená-las em uma string:
```C++
std::string linha;

while (getline(arquivo, linha)) {
    // faça algo com a linha lida
}
```
Após finalizar a leitura do arquivo, é importante fechá-lo utilizando o método "close()":
```C++
arquivo.close();
```

Um exemplo completo de código para ler um arquivo de texto com 5 linhas e imprimir seu conteúdo no console pode ser encontrado [aqui](https://gist.github.com/ExampleUser/123456789).

## Mergulho profundo

Além da leitura de linhas, é possível ler um arquivo de texto caractere por caractere utilizando o método "get()". Também é possível escrever em um arquivo de texto utilizando o método "put()". Outros métodos úteis para manipulação de arquivos de texto incluem "seekg()", para alterar a posição de leitura no arquivo, e "tellg()", para retornar essa posição atual.

Outra forma de ler e escrever em arquivos de texto em C++ é utilizando a biblioteca "cstdio", que possui funções como "fscanf()" e "fprintf()". Porém, essa abordagem é mais antiga e não permite a manipulação de objetos, exigindo o uso de funções específicas para cada tipo de dado.

## Veja também

- [Documentação oficial do C++ sobre manipulação de arquivos](https://en.cppreference.com/w/cpp/io)
- [Tutorial em português sobre leitura e escrita de arquivos em C++](https://www.inf.pucrs.br/~pinho/PRGSWB/Apostila-Arquivo.pdf)
- [Exemplos práticos de manipulação de arquivos em C++](https://www.ime.usp.br/~pf/algoritmos/apendice/apendiceA.pdf)