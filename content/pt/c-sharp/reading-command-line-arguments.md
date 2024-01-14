---
title:                "C#: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em C#

Ler argumentos de linha de comando é um aspecto fundamental em muitas aplicações de C#. Ele permite que os usuários forneçam informações dinamicamente e personalizem a execução do programa de acordo com suas necessidades.

## Como ler argumentos de linha de comando em C#

Para ler argumentos de linha de comando em C#, primeiro é necessário definir uma função Main e, em seguida, utilizar o objeto `args` como parâmetro. Por exemplo:

```C#
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

Neste exemplo, a lista `args` é percorrida utilizando um loop `foreach` e cada argumento é impresso na tela. É importante lembrar que os argumentos são lidos como strings, então é necessário fazer o casting caso sejam necessários outros tipos de dados.

## Aprofundando-se na leitura de argumentos de linha de comando

Além de ler os argumentos fornecidos pelo usuário, também é possível realizar outras operações, como verificar se determinados argumentos foram passados ou definir valores padrão para argumentos não obrigatórios. Também é importante se certificar de que os argumentos inseridos são válidos e lidar com erros caso contrário.

Outra dica importante é utilizar as classes do namespace `System.CommandLine` para facilitar a leitura e o processamento dos argumentos de linha de comando.

## Veja também

- [Documentação oficial do C# sobre argumentos de linha de comando](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/main-and-command-args/command-arguments)
- [Tutorial sobre como ler e processar argumentos de linha de comando em C#](https://www.guru99.com/c-sharp-get-arguments-and-command-line.html)
- [Vídeo tutorial sobre leitura de argumentos de linha de comando em C#](https://www.youtube.com/watch?v=AMcPZKtw9tg)