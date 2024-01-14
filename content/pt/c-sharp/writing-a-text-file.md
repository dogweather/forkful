---
title:    "C#: Escrevendo um arquivo de texto."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

O processo de escrever um arquivo de texto é uma tarefa comum na programação, especialmente em projetos que envolvem a manipulação de dados. Usando a linguagem de programação C#, é possível criar e editar arquivos de texto de forma eficiente e fácil. Neste artigo, veremos por que escrever um arquivo de texto pode ser útil e como fazê-lo usando C#.

## Como escrever um arquivo de texto em C#

Para escrever um arquivo de texto em C#, primeiro precisamos criar um objeto do tipo `StreamWriter`, que é responsável por escrever dados em um arquivo. Precisamos especificar o caminho e nome do arquivo que será criado, junto com o modo de escrita. Aqui está um exemplo de código:

```C#
using System;
using System.IO;
 
class Program
{
    static void Main(string[] args)
    {
        // criando o objeto StreamWriter
        StreamWriter arquivo = new StreamWriter("arquivo.txt", true);
 
        // escrevendo texto no arquivo
        arquivo.WriteLine("Este é um exemplo de texto que será escrito em um arquivo.");
 
        // fechando o arquivo
        arquivo.Close();
 
        // exibindo mensagem de conclusão
        Console.WriteLine("O texto foi escrito com sucesso no arquivo.");
    }
}
```

Após a execução deste código, um novo arquivo chamado "arquivo.txt" será criado na pasta em que o programa estiver sendo executado. O texto especificado no código será escrito no arquivo e, se o arquivo já existir, o texto será adicionado ao final do arquivo. Caso contrário, um novo arquivo será criado com o texto especificado.

## Profundidade de um arquivo de texto

Além de simplesmente escrever texto em um arquivo, é possível realizar outras operações, como ler e editar arquivos de texto utilizando C#. Para isso, é necessário usar os métodos disponíveis na classe `File` ou na classe `StreamReader`. Com essas funcionalidades, podemos criar aplicativos que manipulam arquivos de texto de maneira dinâmica e eficiente.

Uma importante consideração ao trabalhar com arquivos de texto é garantir que eles sejam fechados corretamente após a finalização do processo de escrita ou leitura. Isso pode ser feito usando a instrução `using`, que garante que o recurso do arquivo seja liberado após a sua utilização. Aqui está um exemplo de código para leitura de um arquivo de texto usando `StreamReader`:

```C#
using System;
using System.IO;
 
class Program
{
    static void Main(string[] args)
    {
        // criando o objeto StreamReader
        using (StreamReader arquivo = new StreamReader("arquivo.txt"))
        {
            // lendo o conteúdo do arquivo linha por linha
            while (arquivo.Peek() >= 0)
            {
                string linha = arquivo.ReadLine();
                Console.WriteLine(linha);
            }
        }
    }
}
```

Neste exemplo, usamos a instrução `using` para abrir e fechar automaticamente o arquivo após a sua utilização, e o método `Peek()` é usado para verificar se há conteúdo a ser lido no arquivo.

## Veja também

- [Documentação oficial sobre o uso de arquivos de texto em C#](https://docs.microsoft.com/pt-br/dotnet/standard/io/how-to-write-text-to-a-file)
- [Tutorial sobre leitura de arquivos de texto em C#](https://www.c-sharpcorner.com/UploadFile/mrc20sf/reading-files-using-C-Sharp/)
- [Exemplos de manipulação de arquivos de texto em C#](https://www.codemag.com/Article/1305021/The-Basics-of-Parsing-Text-Files-With-C)