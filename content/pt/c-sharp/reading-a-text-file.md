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

## O que & Porquê?

Ler um arquivo de texto é simplesmente acessar e extrair informações de um arquivo contendo texto legível. Usando a linguagem de programação C#, os programadores podem facilmente ler e manipular arquivos de texto para obter dados importantes do arquivo.

## Como fazer:

Para ler um arquivo de texto usando C#, podemos seguir os seguintes passos:

1. Usar o namespace `System.IO` para acessar as funções de entrada e saída.
2. Usar o método `File.ReadAllText()` para ler o conteúdo do arquivo e armazená-lo em uma variável.
3. Em seguida, podemos manipular os dados de acordo com nossas necessidades.

Aqui está um exemplo de código que mostra como ler um arquivo de texto e imprimir seu conteúdo no console:

```C#
using System.IO;
using System;

class Program
{
    static void Main()
    {
        string texto = File.ReadAllText("arquivo.txt");
        Console.WriteLine(texto);
    }
}
```

A saída seria o conteúdo do arquivo de texto `arquivo.txt` impresso no console.

## Deep Dive:

Ler um arquivo de texto é uma tarefa comum para os programadores, pois os arquivos de texto são amplamente usados ​​para armazenar dados simples em um formato legível. Ele pode ser usado para armazenar nomes de usuários, senhas, configurações do software e muito mais. Além disso, é uma forma rápida e fácil de compartilhar dados entre diferentes sistemas.

No passado, os arquivos de texto eram lidos usando comandos do sistema operacional específicos da plataforma. Com o surgimento da linguagem de programação C# e o uso do namespace `System.IO`, esse processo se tornou mais padronizado e fácil para os desenvolvedores. Além disso, existem outras alternativas para ler arquivos de texto, como o uso de bibliotecas de terceiros.

Para implementar a leitura de arquivos de texto em um aplicativo C#, é importante entender a estrutura básica dos arquivos de texto, como a codificação de caracteres usada e a diferença entre arquivos de texto e arquivos binários.

## Veja também:

- [Documentação da classe System.IO](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file)
- [Tutorial sobre leitura de arquivos de texto em C#](https://www.tutorialsteacher.com/csharp/csharp-read-text-file)
- [Manipulando arquivos de texto com C#](https://www.c-sharpcorner.com/UploadFile/1bf814/reading-and-writing-text-file-in-C-Sharp/)