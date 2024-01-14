---
title:                "C#: Criando um arquivo temporário"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em C#?

Criar um arquivo temporário é uma tarefa comum para muitos desenvolvedores em C#. Pode ser necessário para armazenar dados temporários enquanto o programa está em execução ou para compartilhar informações entre diferentes processos. Em geral, é uma maneira útil de gerenciar dados temporários sem a necessidade de criar arquivos permanentes que precisam ser constantemente mantidos e atualizados.

## Como criar um arquivo temporário em C#

Para criar um arquivo temporário em C#, podemos usar a classe `System.IO.Path` e o método `GetTempFileName()`. Este método cria um arquivo temporário com um nome único e retorna o caminho completo para o arquivo. Podemos usar este caminho para armazenar e manipular os dados temporários, como no exemplo abaixo:

```C#
var caminhoArquivo = Path.GetTempFileName();
Console.WriteLine($"Caminho do arquivo temporário: {caminhoArquivo}");
```

A saída deste código seria:

```
Caminho do arquivo temporário: C:\Users\nomedousuario\AppData\Local\Temp\tmpFD3D.tmp
```

## Mergulho profundo na criação de arquivos temporários

Ao criar um arquivo temporário, é importante ter em mente que ele será excluído automaticamente quando o programa terminar ou quando o arquivo for fechado. Podemos também especificar um diretório para armazenar o arquivo temporário usando o método `GetTempFileName()`.

Além disso, podemos utilizar a classe `System.IO.File` para escrever, ler ou excluir o conteúdo do arquivo temporário. Como alternativa, também podemos usar a classe `FileStream` para operações mais avançadas no arquivo, como alterar permissões ou definir o arquivo como somente leitura.

## Veja também

- [Documentação oficial da classe Path](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path?view=netframework-4.8)
- [Documentação oficial da classe File](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file?view=netframework-4.8)
- [Documentação oficial da classe FileStream](https://docs.microsoft.com/pt-br/dotnet/api/system.io.filestream?view=netframework-4.8)