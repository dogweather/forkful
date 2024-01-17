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

## O que & Por quê?
Criar um arquivo temporário é uma técnica frequente de programação em que um arquivo é criado para armazenar temporariamente dados durante a execução do programa. Isso é útil quando os dados precisam ser acessados e atualizados rapidamente sem sobrecarregar o sistema ou a memória. Programadores usam essa técnica para criar arquivos temporários para armazenar informações como logs, cache, e outras informações temporárias.

## Como fazer:
```C#
// Exemplo de criação de arquivo temporário
// Usando a classe Path
string arquivoTemporario = Path.GetTempFileName();

Console.WriteLine("Caminho do arquivo temporário: {0}", arquivoTemporario);
Console.WriteLine("Conteúdo do arquivo:");
Console.WriteLine(File.ReadAllText(arquivoTemporario));

// Exemplo de criação de arquivo temporário com conteúdo específico
// Usando a classe FileStream
string caminho = Path.GetTempFileName();
string conteudo = "Este é um arquivo temporário criado usando a classe FileStream.";

// Cria o arquivo temporário
using (FileStream fs = File.Create(caminho))
{
    // Converte a string com o conteúdo para bytes
    byte[] bytes = new UTF8Encoding(true).GetBytes(conteudo);
    // Escreve os bytes no arquivo
    fs.Write(bytes, 0, bytes.Length);
}

// Lê e imprime o conteúdo do arquivo
Console.WriteLine("Conteúdo do arquivo criado com FileStream:");
Console.WriteLine(File.ReadAllText(caminho));
```

## Aprofundamento:
A criação de arquivos temporários é uma técnica comum na programação e tem sido usada há muitos anos, com a criação de arquivos temporários remontando ao início dos anos 1980. Além do uso das classes Path e FileStream, os programadores também podem criar arquivos temporários usando outras classes, como a classe DirectoryInfo do namespace System.IO. Alternativamente, os arquivos temporários podem ser criados em locais específicos definidos pelo sistema operacional, mas isso requer cuidado extra da parte do programador. Ao usar a classe Path, o sistema operacional determina automaticamente o local apropriado para o arquivo temporário. 

## Veja também:
- [Documentação oficial da classe Path](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path?view=netcore-3.1)
- [Documentação oficial da classe FileStream](https://docs.microsoft.com/pt-br/dotnet/api/system.io.filestream?view=netcore-3.1)
- [Documentação oficial da classe File](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file?view=netcore-3.1)
- [Documentação oficial da classe DirectoryInfo](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directoryinfo?view=netcore-3.1)