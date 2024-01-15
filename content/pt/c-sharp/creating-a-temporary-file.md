---
title:                "Criando um arquivo temporário"
html_title:           "C#: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário no C#?

Criar um arquivo temporário pode ser útil em diversas situações, como quando precisamos armazenar dados temporariamente ou gerar relatórios dinamicamente. Além disso, é uma maneira eficiente de gerenciar espaços de armazenamento e garantir que não haja conflitos de nomes de arquivos.

## Como criar um arquivo temporário no C#

Para criar um arquivo temporário no C#, podemos utilizar a classe `Path` e o método `GetTempFileName()`. Isso criará um arquivo temporário no diretório padrão de arquivos temporários do sistema operacional.

```C#
var nomeArquivo = Path.GetTempFileName();
Console.WriteLine($"Arquivo temporário criado: {nomeArquivo}");
```

O código acima irá gerar um nome de arquivo com uma extensão `.tmp` e o caminho completo do arquivo será retornado pelo método `GetTempFileName()`. Podemos então utilizar esse nome para manipular o arquivo conforme necessário.

## Mais informações sobre a criação de arquivos temporários

Ao utilizar o método `GetTempFileName()`, é importante lembrar que o arquivo temporário gerado será automaticamente excluído quando o aplicativo for encerrado. Caso seja necessário manter o arquivo após o encerramento do programa, é possível utilizar o método `GetTempPath()` da classe `Path` para obter o diretório padrão de arquivos temporários e criar um arquivo com um nome específico.

```C#
var caminhoTemp = Path.GetTempPath();
var nomeArquivo = "meuarquivotemporario.txt";
var caminhoCompleto = Path.Combine(caminhoTemp, nomeArquivo);

FileStream fileStream = File.Create(caminhoCompleto);
fileStream.Close();

Console.WriteLine($"Arquivo temporário criado em: {caminhoCompleto}");
```

Com isso, o arquivo temporário será criado em um local específico e não será excluído automaticamente após o encerramento do programa.

## Veja também

- Documentação oficial do método `GetTempFileName()` em C#: [https://docs.microsoft.com/pt-br/dotnet/api/system.io.path.gettempfilename?view=net-5.0](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- Tutorial sobre a criação de arquivos temporários em C#: [https://www.tutorialsteacher.com/csharp/create-temp-file](https://www.tutorialsteacher.com/csharp/create-temp-file)
- Artigo sobre a importância de gerenciar arquivos temporários em aplicativos: [https://www.ghacks.net/2020/02/16/what-you-should-know-about-temporary-files-on-windows/](https://www.ghacks.net/2020/02/16/what-you-should-know-about-temporary-files-on-windows/)