---
title:    "C#: Lendo um arquivo de texto"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa comum na programação, especialmente quando lidamos com dados que não estão armazenados em um banco de dados ou em formato estruturado. É importante saber como ler um arquivo de texto para poder acessar e manipular esses dados importantes.

## Como ler um arquivo de texto em C#

Existem várias maneiras de ler um arquivo de texto em C#, mas vamos mostrar a maneira mais simples e comum usando a classe `StreamReader`. Primeiro, precisamos criar uma instância dessa classe e passar o caminho do arquivo como argumento:

```C#
StreamReader leitor = new StreamReader("caminho/do/arquivo.txt");
```

Em seguida, podemos usar o método `ReadLine()` para ler uma linha do arquivo de cada vez e armazenar em uma variável:

```C#
string linha = leitor.ReadLine();
```

Podemos repetir esse processo quantas vezes forem necessárias para ler todas as linhas do arquivo. É importante fechar o leitor ao final do processo usando o método `Close()` para liberar os recursos do sistema.

```C#
leitor.Close();
```

Aqui está um exemplo completo de como ler um arquivo de texto e imprimir seu conteúdo no console:

```C#
using System;
using System.IO;

StreamReader leitor = new StreamReader("caminho/do/arquivo.txt");

string linha;
while ((linha = leitor.ReadLine()) != null)
{
    Console.WriteLine(linha);
}

leitor.Close();
```

O resultado desse código seria a impressão de todas as linhas do arquivo de texto no console.

## Aprofundando um pouco mais

Além da classe `StreamReader`, existem outras formas de ler arquivos de texto em C#. Podemos usar a classe `File` ou `FileInfo` para acessar informações sobre o arquivo e ler seu conteúdo. Também podemos usar o método `ReadAllText()` para ler todo o conteúdo do arquivo de uma só vez.

Para manipular os dados lidos, podemos usar as classes `StreamWriter` e `FileWriter` para escrever em um arquivo de texto. Existem também métodos para verificar e criar arquivos, além de transformar o arquivo de texto em um array de strings usando o método `ReadAllLines()`

## Veja também

- [Documentação oficial sobre leitura de arquivos em C#](https://docs.microsoft.com/pt-br/dotnet/standard/io/how-to-read-text-from-a-file)
- [Tutorial em vídeo sobre leitura de arquivos em C#](https://www.youtube.com/watch?v=n_lSuWZ3qZA)
- [Exemplos de código para ler e escrever em arquivos de texto em C#](https://gist.github.com/rmtheis/11393602)