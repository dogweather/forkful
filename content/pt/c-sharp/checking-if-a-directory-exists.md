---
title:                "Verificando se um diretório existe"
html_title:           "C#: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar se um diretório existe é o processo de verificar se um caminho específico em seu sistema de arquivos contém um diretório. Os programadores fazem isso para evitar erros ao tentar acessar ou manipular um diretório que não existe.

## Como Fazer:

Aqui está um exemplo de como você pode verificar se um diretório existe em C#:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\temp";

        if (Directory.Exists(path))
        {
            System.Console.WriteLine("O diretório existe.");
        }
        else
        {
            System.Console.WriteLine("O diretório não existe.");
        }
    }
}
```

Se o diretório "C:\temp" existir, o console imprimirá "O diretório existe.". Caso contrário, ele imprimirá "O diretório não existe.".

## Deep Dive:

Historicamente, a verificação da existência de um diretório em C# é feita usando o método 'Directory.Exists()'. Isso tem sido parte do .NET desde a sua criação.

Como alternativa, você pode manipular a exceção `DirectoryNotFoundException` ao tentar abrir um diretório inexistente. No entanto, é mais eficiente e limpo verificar a existência do diretório apropriadamente antes de tentar operações nele.

A implementação interna do `Directory.Exists()` usa P/Invoke para chamar a função `GetFileAttributesW` da API Win32. Verifica-se se o valor retornado tem o bit 'FILE_ATTRIBUTE_DIRECTORY' definido. Em uma máquina POSIX, ela faz um syscall 'stat' e verifica se o tipo de arquivo é um diretório.

## Veja também:

Para saber mais sobre como verificar se um diretório existe em C#, você pode visitar os seguintes links:

1. Documentação Microsoft Directory.Exists: [https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory.exists](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory.exists)

2. StackOverflow - Como verificar se um diretório existe em C#: [https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory)