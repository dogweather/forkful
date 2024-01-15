---
title:                "Lendo um arquivo de texto"
html_title:           "C#: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Ler e escrever arquivos de texto é uma tarefa comum na programação em C# e é algo que todo desenvolvedor precisa saber. Este artigo irá explicar o porquê e como ler arquivos de texto usando a linguagem C#, fornecendo exemplos de código e informações mais detalhadas para aprofundamento.

## Como fazer

Para ler um arquivo de texto em C#, primeiro é necessário declarar uma variável do tipo `StreamReader` e atribuir a ela o caminho do arquivo desejado. Em seguida, use o método `ReadLine()` para ler o conteúdo do arquivo linha por linha. Veja um exemplo abaixo:

```C#
StreamReader leitor = new StreamReader("caminho/do/seu/arquivo.txt");
string linha;
while ((linha = leitor.ReadLine()) != null) 
{
    Console.WriteLine(linha);
}
leitor.Close();
```

Este código irá imprimir todas as linhas do arquivo de texto na tela. Lembre-se de sempre fechar o `StreamReader` após a leitura do arquivo, utilizando o método `Close()`.

Também é possível utilizar o método `ReadToEnd()` para ler o arquivo inteiro de uma só vez e atribuí-lo a uma variável do tipo `string`. Veja um exemplo abaixo:

```C#
StreamReader leitor = new StreamReader("caminho/do/seu/arquivo.txt");
string conteudo = leitor.ReadToEnd();
leitor.Close();
```
Neste caso, todo o conteúdo do arquivo será armazenado na variável `conteudo` e poderá ser utilizado conforme necessário.

É importante lembrar que ao ler um arquivo de texto, é necessário estar atento aos caracteres especiais, como `"\n"` para quebras de linha ou `"\t"` para tabulações.

## Aprofundando-se

Ao trabalhar com a leitura de arquivos de texto, é importante estar ciente de que existem diferentes formas de ler e manipular o conteúdo. Por exemplo, ao utilizar o `StreamReader`, os arquivos serão lidos de forma síncrona, ou seja, uma linha de cada vez. Por outro lado, é possível utilizar o `Stream` para ler de forma assíncrona, permitindo que o processo de leitura seja executado em paralelo com outras tarefas.

Também é importante lembrar que a leitura de arquivos de texto pode ser combinada com outras operações, como a manipulação de strings ou conversão de dados. Cada caso pode exigir uma abordagem diferente, portanto, é sempre bom consultar a documentação oficial do C# para encontrar a melhor solução.

## Veja também

- Documentação oficial do C#: [https://docs.microsoft.com/pt-br/dotnet/csharp/](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- Leitura e escrita de arquivos de texto em C#: [https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- Manipulação de strings em C#: [https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/)