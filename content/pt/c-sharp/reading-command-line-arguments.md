---
title:                "Lendo argumentos da linha de comando"
html_title:           "C#: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando em C#?

Se você está familiarizado com programação em C#, provavelmente já sabe que a linguagem oferece diversas opções para ler dados de entrada. No entanto, a leitura de argumentos da linha de comando pode ser particularmente útil em alguns casos específicos. Neste artigo, vamos explorar por que pode ser interessante incluir esse recurso em suas aplicações.

## Como fazer a leitura de argumentos da linha de comando em C#

Em C#, a leitura de argumentos passados pela linha de comando é feita utilizando a classe `Environment` e seus métodos `GetCommandLineArgs()` e `GetFolderPath()`. Vamos ver um exemplo prático de como utilizar esses recursos:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // Obtém todos os argumentos passados pela linha de comando
        string[] arguments = Environment.GetCommandLineArgs();

        // Exibe os argumentos na tela
        Console.WriteLine("Os argumentos passados foram:");
        foreach (string arg in arguments)
        {
            Console.WriteLine(arg);
        }
    }
}
```

No exemplo acima, ao executar o programa com `dotnet run argumento1 argumento2`, o resultado será:

```
Os argumentos passados foram:
/path/to/program.exe
argumento1
argumento2
```

Um ponto importante a ser destacado é que o primeiro argumento retornado pelo método `GetCommandLineArgs()` é sempre o caminho para o programa em si. Portanto, devemos estar cientes dessa informação para tratá-lo adequadamente. Além disso, é possível obter o diretório atual do programa utilizando o método `GetFolderPath()` e passando o argumento `Environment.SpecialFolder.CurrentDirectory`.

## Mergulhando mais fundo nos argumentos da linha de comando

Vale mencionar que há diversas opções para obter informações sobre os argumentos da linha de comando em C#. Além do método `GetCommandLineArgs()`, podemos utilizar as classes `Console` e `CommandLine` que oferecem recursos mais avançados.

Por exemplo, a classe `Console` possui um método `Read` que permite fazer a leitura de entrada diretamente pela linha de comando. Já a classe `CommandLine` pode ser utilizada para extrair informações específicas dos argumentos, como separar valores a partir de um caractere específico ou ignorar determinados argumentos.

De qualquer forma, a leitura de argumentos da linha de comando é uma funcionalidade importante para aumentar a flexibilidade e interatividade de suas aplicações em C#.

## Veja também

- [Documentação da classe Environment em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.environment)
- [Documentação da classe Console em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.console)
- [Documentação da classe CommandLine em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.environment.commandline)