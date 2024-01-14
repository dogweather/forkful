---
title:                "C#: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao escrever um programa em C# que lida com arquivos e diretórios, pode ser necessário verificar se um diretório especificado pelo usuário realmente existe. Isso pode ser útil na validação de entradas ou na garantia de que o programa não tente acessar um diretório inexistente.

## Como fazer:

Para verificar se um diretório existe, podemos usar o método "Directory.Exists" da classe "System.IO". Este método recebe como argumento uma string que representa o caminho do diretório a ser verificado e retorna um valor booleano indicando se o diretório existe ou não.

```C#
if (Directory.Exists("C:\MeusDocumentos\Imagens"))
{
    Console.WriteLine("O diretório existe!");
}
else
{
    Console.WriteLine("O diretório não existe!");
}
```

A saída deste código será "O diretório existe!", caso o diretório "Imagens" exista na pasta "Meus Documentos". Caso contrário, a saída será "O diretório não existe!".

## Profundidade:

O método "Directory.Exists" usa a classe "DirectoryInfo" por trás dos bastidores para realizar a verificação. Esta classe também possui outros métodos úteis para a manipulação de diretórios, como criar, mover e excluir diretórios.

Além disso, é importante lembrar que, mesmo que o diretório exista no momento da verificação, ele pode ser excluído posteriormente pelo usuário ou por outro programa. Por isso, é preciso ter cuidado ao lidar com arquivos e diretórios no seu código e sempre tratar possíveis erros.

## Veja também:

- [Documentação do método Directory.Exists em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [Exemplos práticos de uso da classe DirectoryInfo](https://www.edureka.co/blog/directoryinfo-class-in-c-sharp/)