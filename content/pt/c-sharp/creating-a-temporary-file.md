---
title:    "C#: Criando um arquivo temporário"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em C#?

Criar arquivos temporários pode ser útil em diversas situações durante o desenvolvimento de um programa em C#. Eles permitem que você armazene informações temporariamente e as acesse facilmente, sem a necessidade de lidar com a criação e manutenção de um arquivo permanente. Além disso, arquivos temporários podem ser excluídos quando não são mais necessários, economizando recursos do sistema.

## Como criar um arquivo temporário em C#

Para criar um arquivo temporário em C#, você pode utilizar a classe `Path` e o método `GetTempFileName`, que irá gerar um nome aleatório para o arquivo temporário. Veja um exemplo abaixo:

```C#
string nomeArquivo = Path.GetTempFileName();
Console.WriteLine("Arquivo temporário criado: " + nomeArquivo);
```

A saída deste código será algo como: `Arquivo temporário criado: C:\Users\Usuario\AppData\Local\Temp\reyh2tzi.tmp`.

## Mais informações sobre criação de arquivos temporários

Além de utilizar o método `GetTempFileName`, você também pode criar um arquivo temporário especificando o diretório em que ele será criado, assim como seu nome e extensão. Para isso, é necessário utilizar a classe `Path` e o método `Combine`, como mostrado no exemplo abaixo:

```C#
string nomeArquivo = Path.Combine(Path.GetTempPath(), "meuarquivo.txt");
File.Create(nomeArquivo).Close();
Console.WriteLine("Arquivo temporário criado: " + nomeArquivo);
```

Neste exemplo, o arquivo temporário será criado na pasta padrão de arquivos temporários do sistema operacional, com o nome "meuarquivo.txt". Além disso, é importante ressaltar que é necessário fechar o arquivo criado após sua criação, para que ele possa ser usado posteriormente.

# Veja também

- [Documentação oficial do método GetTempFileName em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8)
- [Exemplo prático de uso de arquivos temporários em C#](https://www.devmedia.com.br/trabalhando-com-arquivos-temporarios-em-csharp/37702)