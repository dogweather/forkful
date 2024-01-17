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

## O que & Porquê?
Verificar se um diretório existe é um processo importante para os programadores, pois permite que eles garantam a integridade e a organização do sistema de arquivos em um aplicativo. Isso também ajuda a evitar erros e falhas no programa.

## Como fazer:
Verificar se um diretório existe é uma tarefa simples e direta no C#. Primeiro, é necessário importar o namespace System.IO para ter acesso às classes de gerenciamento de arquivos. Em seguida, utilizamos o método `Directory.Exists(path)` para verificar se o diretório especificado pelo caminho `path` realmente existe. Se o diretório existir, o método retornará `true`, caso contrário, retornará `false`.

```
C# using System.IO; // importando o namespace
string path = "./meuDiretorio"; // especificando o caminho do diretório
if (Directory.Exists(path)) // verificando se o diretório existe
{
    Console.WriteLine("O diretório existe!"); // impressão de mensagem caso exista
}
else
{
    Console.WriteLine("O diretório não existe!"); // impressão de mensagem caso não exista
}
```

Exemplo de saída:

```
O diretório existe!
```

## Mergulho profundo:
No passado, antes da introdução do `Directory.Exists(path)` no .NET Framework 2.0, os programadores precisavam usar a classe `DirectoryInfo` para verificar se um diretório existia. No entanto, essa classe tem uma sobrecarga desnecessária, pois também fornece funcionalidade para criar e excluir diretórios. O que torna o método `Directory.Exists(path)` mais eficiente é que ele usa o método nativo do sistema operacional para verificar a existência do diretório. No entanto, é importante ter em mente que o método pode retornar `false` se houver permissões insuficientes para acessar o diretório.

## Veja também:
- Documentação oficial da classe Directory no C# (em inglês): https://docs.microsoft.com/en-us/dotnet/api/system.io.directory
- Mais informações sobre a classe FileSystemInfo no C# (em português): https://docs.microsoft.com/pt-br/dotnet/api/system.io.filesysteminfo?view=netcore-3.1