---
title:    "C#: Lendo argumentos de linha de comando"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando é importante

Ler argumentos de linha de comando é uma habilidade fundamental para qualquer programador C#. Isso permite que você crie programas dinâmicos e interativos, permitindo que os usuários forneçam entradas personalizadas e manipulem o comportamento do programa. Neste artigo, vamos aprender por que ler argumentos de linha de comando é importante e como fazer isso usando C#.

## Como ler argumentos de linha de comando em C#

Ler argumentos de linha de comando é uma tarefa relativamente simples em C#, mas requer a utilização de alguns conceitos e funções específicas. Vamos ver um exemplo de código e sua saída para entender melhor o processo.

```C#
using System; 

class Program 
{ 
    static void Main(string[] args) 
    { 
        Console.WriteLine("Número de argumentos: " + args.Length);
 
        for (int i = 0; i < args.Length; i++) 
        { 
            Console.WriteLine("Argumento {0}: {1}", i + 1, args[i]); 
        } 
    } 
}
```

Exemplo de saída:

```
Número de argumentos: 3
Argumento 1: Olá
Argumento 2: Mundo
Argumento 3: !
```

Neste exemplo, usamos o método `Main()` para receber os argumentos passados na linha de comando. Em seguida, usamos `Console.WriteLine()` para exibir a quantidade de argumentos e o conteúdo de cada um deles. É possível acessar cada argumento individualmente usando seu índice dentro do array `args`.

## Deep Dive na leitura de argumentos de linha de comando

Ler argumentos de linha de comando pode se tornar ainda mais poderoso quando combinado com outras funcionalidades do C#. Por exemplo, podemos usar a classe `Console` para solicitar ao usuário um argumento específico, caso ele não tenha sido fornecido na linha de comando. Além disso, podemos usar o método `Environment.GetCommandLineArgs()` para obter todo o comando de linha digitado pelo usuário, incluindo o nome do programa.

O uso de argumentos de linha de comando também pode ser extendido para a leitura de arquivos de configuração ou parametrização de scripts de automação. Com um conhecimento básico sobre leitura de argumentos de linha de comando, você pode facilmente adaptá-lo para suas necessidades específicas.

## Veja também

- [Documentação oficial do C# sobre argumentos de linha de comando](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Tutorial sobre como ler argumentos de linha de comando em C#](https://www.c-sharpcorner.com/UploadFile/syedshakeer/command-line-arguments-in-C-Sharp/)
- [Exemplo de uso de argumentos de linha de comando em um programa de console C#](https://www.geeksforgeeks.org/c-sharp-complete-parameters-command-line-arguments-and-variable-argument-list/)

Com esses recursos, você estará pronto para utilizar argumentos de linha de comando em seus projetos e aproveitar ao máximo essa funcionalidade do C#!