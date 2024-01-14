---
title:                "C#: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Por que

Criar arquivos temporários é uma tarefa comum quando se trabalha com programação. Eles são úteis para armazenar informações temporárias, realizar operações rápidas ou testar novas funcionalidades sem afetar o código principal. Neste artigo, vamos explorar como criar arquivos temporários em C# e como eles podem ser úteis no desenvolvimento de software.

##Como Fazer

Existem várias maneiras de criar arquivos temporários em C#. A mais simples delas é utilizando o método GetTempFileName() da classe Path. Este método cria um arquivo temporário no diretório temporário do sistema e retorna o caminho absoluto para esse arquivo.

```C#
string tmpFile = Path.GetTempFileName();
Console.WriteLine(tmpFile);
```

A saída deste código será algo como "C:\Users\Usuario\AppData\Local\Temp\tmp2661.tmp". Note que o nome do arquivo é gerado de forma aleatória para garantir que ele seja único.

Outra forma de criar arquivos temporários é utilizando a classe File do namespace System.IO. Com ela, podemos utilizar o método CreateTempFile(), que funciona de maneira semelhante ao GetTempFileName().

```C#
string tmpFile = Path.Combine(Path.GetTempPath(), "myfile.tmp");
File.Create(tmpFile);
```

Neste caso, estamos criando o arquivo temporário no diretório temporário especificado pelo método GetTempPath() e atribuindo um nome customizado ao arquivo.

##Mergulho Profundo

Ao criar um arquivo temporário, é importante prestar atenção à sua segurança e gerenciamento. Uma boa prática é sempre garantir que o arquivo seja excluído após o seu uso, para evitar que ele continue ocupando espaço no sistema. Isso pode ser feito utilizando o método Delete() da classe File.

```C#
string tmpFile = Path.GetTempFileName();
File.Delete(tmpFile);
```

Além disso, é importante estar ciente de que o sistema operacional pode excluir o arquivo temporário automaticamente se ele não for utilizado por um período de tempo. Por isso, é recomendável sempre utilizar o arquivo dentro de um fluxo de código, para garantir que ele seja mantido ativo pelo sistema.

##Veja Também

- [Microsoft Documentação sobre o método GetTempFileName() ](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path.gettempfilename)
- [Microsoft Documentação sobre o método CreateTempFile() ](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file.createtempfile)
- [Artigo sobre práticas de segurança com arquivos temporários em C#](https://blog.elonsoftwares.com.br/seguranca-com-arquivos-temporarios-em-c/)