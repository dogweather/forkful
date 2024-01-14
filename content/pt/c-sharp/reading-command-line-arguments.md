---
title:                "C#: Lendo argumentos da linha de comando"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em C#?

Você já se deparou com a necessidade de fornecer informações adicionais ao seu programa durante sua execução? Nesse caso, a leitura de argumentos da linha de comando pode ser a solução ideal. 

## Como fazer

Antes de tudo, vamos entender o que são argumentos da linha de comando. Esses são valores ou opções passados ao programa através da linha de comando, quando ele é executado. Pode ser útil para personalizar a execução do programa ou para fornecer informações para o mesmo.

Para ler os argumentos da linha de comando em C#, você pode utilizar a classe `Environment` e o método `GetCommandLineArgs()`. Vamos ver um exemplo:

```
C#
class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Argumentos passados: ");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Ao executar o programa acima com os argumentos `arg1 arg2 arg3`, o resultado será:

```
Argumentos passados:
arg1
arg2
arg3
```

**Observação:** o primeiro argumento retornado pelo método `GetCommandLineArgs()` geralmente é o caminho do executável, por isso o loop começa a partir do segundo índice (`args[1]`).

## Mergulho Profundo

Além do método `GetCommandLineArgs()`, também é possível utilizar outras abordagens para ler argumentos da linha de comando em C#, como o uso da classe `CommandLine` do pacote NuGet `Command Line Parser`.

Além disso, é importante saber que é possível passar argumentos com valores para o programa, utilizando a sintaxe `nome_do_argumento=valor`, e depois convertê-los para o tipo desejado no código.

## Veja também

- [Documentação oficial do método `GetCommandLineArgs()` em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Pacote NuGet `Command Line Parser` para facilitar a leitura de argumentos da linha de comando em C#](https://www.nuget.org/packages/CommandLineParser/)