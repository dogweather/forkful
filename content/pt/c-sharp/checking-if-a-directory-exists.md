---
title:                "Verificando se um diretório existe."
html_title:           "C#: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como um programa pode saber se um determinado diretório existe ou não no computador? Bem, neste artigo vamos explorar o processo de como um desenvolvedor pode verificar isso usando a linguagem de programação C#. 

## Como fazer

Para verificar se um diretório existe em C#, podemos usar o método `Directory.Exists()` da classe `System.IO`. Este método retorna um valor booleano indicando se o diretório especificado existe ou não.

```C#
if (Directory.Exists("C:\\Users\\Admin\\Documents"))
{
    Console.WriteLine("O diretório existe!");
}
else
{
    Console.WriteLine("O diretório não existe!");
}
```

Ao executar este código, a saída será "O diretório existe!" caso o diretório exista no caminho especificado. Caso contrário, a saída será "O diretório não existe!".

## Deep Dive

Por trás dos panos, o método `Directory.Exists()` chama a função `PathInternal.IsDirectoryExists()` do sistema operacional. Esta função é responsável por verificar se o diretório existe.

É importante observar que o método `Directory.Exists()` não verifica se o usuário tem permissão para acessar o diretório, apenas se ele existe ou não. Além disso, este método não funciona para diretórios em unidades de rede.

## Veja também

Se você quiser aprender mais sobre o uso de diretórios em C#, confira as seguintes referências:

- [Documentação da classe `Directory`](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory?view=net-5.0)
- [Guia de C# em português](https://docs.microsoft.com/pt-br/dotnet/csharp/index)

Agora você sabe como verificar se um diretório existe em C#! Experimente usar este conhecimento em seus projetos e veja como ele pode melhorar a funcionalidade do seu programa. Divirta-se codificando!