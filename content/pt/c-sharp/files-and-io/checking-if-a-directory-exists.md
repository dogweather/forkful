---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:05.609809-07:00
description: "Como Fazer: C# fornece o namespace `System.IO`, que cont\xE9m a classe\
  \ `Directory`, oferecendo uma maneira direta de verificar a exist\xEAncia de um\
  \ diret\xF3rio\u2026"
lastmod: '2024-03-13T22:44:46.598204-06:00'
model: gpt-4-0125-preview
summary: "C# fornece o namespace `System.IO`, que cont\xE9m a classe `Directory`,\
  \ oferecendo uma maneira direta de verificar a exist\xEAncia de um diret\xF3rio\
  \ por meio do m\xE9todo `Exists`."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como Fazer:


### Usando System.IO
C# fornece o namespace `System.IO`, que contém a classe `Directory`, oferecendo uma maneira direta de verificar a existência de um diretório por meio do método `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Verificar se o diretório existe
        bool directoryExists = Directory.Exists(directoryPath);

        // Imprimir o resultado
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Saída de Exemplo:**

```
Directory exists: False
```

Caso o diretório realmente exista no caminho `C:\ExampleDirectory`, a saída será `True`.

### Usando System.IO.Abstractions para testes unitários
Quando se trata de tornar seu código testável por unidade, especialmente quando ele interage com o sistema de arquivos, o pacote `System.IO.Abstractions` é uma escolha popular. Ele permite abstrair e simular operações do sistema de arquivos nos seus testes. Veja como você poderia verificar a existência de um diretório usando essa abordagem:

Primeiro, garanta que você instalou o pacote:

```
Install-Package System.IO.Abstractions
```

Depois, você pode injetar um `IFileSystem` na sua classe e usá-lo para verificar se um diretório existe, o que facilita os testes unitários.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Saída de Exemplo:**

```
Directory exists: False
```

Essa abordagem desacopla a lógica da aplicação do acesso direto ao sistema de arquivos, tornando seu código mais modular, testável e sustentável.
