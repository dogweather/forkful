---
title:                "Verificando se um diretório existe"
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Verificar se um diretório existe é uma operação onde checamos se uma pasta específica já está criada no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em diretórios que não existem.

## Como Fazer:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string caminho = @"C:\ExemploDiretorio";
        
        if (Directory.Exists(caminho))
        {
            Console.WriteLine("O diretório existe.");
        }
        else
        {
            Console.WriteLine("O diretório não existe.");
            // Código para criar o diretório, se necessário:
            // Directory.CreateDirectory(caminho);
        }
    }
}
```

Saída esperada (caso o diretório não exista):
```
O diretório não existe.
```

Saída esperada (caso o diretório exista):
```
O diretório existe.
```

## Mergulho Profundo

Verificar a existência de um diretório é uma prática comum desde os primeiros sistemas operacionais que suportavam sistemas de arquivos hierárquicos. No .NET Framework, a classe `System.IO.Directory` fornece métodos estáticos para interagir com diretórios, incluindo `Directory.Exists()`. Alternativas incluem o uso do método `DirectoryInfo.Exists`, que é uma instância um pouco mais orientada a objetos. É importante lembrar de tratar exceções que podem ocorrer devido a permissões de acesso ou erros de I/O.

## Veja Também

- Documentação oficial do método `Directory.Exists`: https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory.exists
- Referência da classe `DirectoryInfo`: https://docs.microsoft.com/pt-br/dotnet/api/system.io.directoryinfo
- Guia de manipulação de arquivos e diretórios no C#: https://docs.microsoft.com/pt-br/dotnet/standard/io/how-to-create-a-directory
- Tratamento de erros I/O em C#: https://docs.microsoft.com/pt-br/dotnet/standard/io/handling-io-errors