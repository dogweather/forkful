---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lendo Argumentos da Linha de Comando em C#

## O que e Por quê?

Ler argumentos da linha de comando é um meio de se obter informações de entrada pelo usuário via terminal ou linha de comando. Programadores usam isso para permitir que os usuários personalizem a execução do programa, passando parâmetros no momento da inicialização.

## Como Fazer:

Aqui está um exemplo de como ler argumentos da linha de comando em C#. O exemplo usa o array `args` definido na função `Main`.

```csharp
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```
Se iniciássemos este programa passando os argumentos "Olá" e "Mundo", a saída seria:

```
Olá
Mundo
```

## Deep Dive:

Historicamente, ler argumentos da linha de comando é uma prática antiga herdada dos sistemas UNIX. Além disso, existem alternativas ao método discutido; a classe `Environment` fornece acesso a detalhes como variáveis de ambiente, versão do runtime .NET e assim por diante.

```csharp
string[] args = Environment.GetCommandLineArgs();
foreach (string arg in args)
{
    Console.WriteLine(arg);
}
```

Na implementação padrão, `args[0]` é o caminho e o nome do arquivo executável. Os argumentos de linha de comando reais começam de `args[1]`.

## Veja Também:

- Documentação Microsoft para o objeto `System.Environment`: [Aqui](https://docs.microsoft.com/pt-br/dotnet/api/system.environment?view=net-5.0)