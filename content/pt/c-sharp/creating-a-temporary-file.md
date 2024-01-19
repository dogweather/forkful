---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Como criar um arquivo temporário em C#

## O Que & Porquê?

Criar um arquivo temporário na programação é basicamente gerar um arquivo que é usado apenas por um curto período de tempo. Os programadores fazem isso frequentemente para armazenar dados que não precisam ser mantidos indefinidamente ou para fins de teste rápido.

## Como Fazer:

Usamos a classe `Path` no namespace `System.IO` para criar arquivos temporários em C#. Veja no exemplo abaixo:

```C#
using System.IO;

public class Program
{
    public static void Main()
    {
        string tempFile = Path.GetTempFileName();

        using (StreamWriter writer = new StreamWriter(tempFile))
        {
            writer.WriteLine("Este é um arquivo temporário!");
        }

        using (StreamReader reader = new StreamReader(tempFile))
        {
            string line = reader.ReadLine();
            Console.WriteLine(line);
        }

        File.Delete(tempFile);
    }
}
```

Aqui, `Path.GetTempFileName()` cria um arquivo temporário e retorna o caminho completo para esse arquivo. A saída seria: `Este é um arquivo temporário!`

## Mergulhando Mais Fundo:

Historicamente, os arquivos temporários são um conceito que tem sido usado desde os primeiros dias da programação. Hoje, em C#, temos diversas formas de lidar com arquivos temporários, incluindo o uso de classes como `Path` e `FileStream`.

Um método alternativo é criar seu próprio gerador de arquivos temporários, embora isso possa não ser necessário a menos que você tenha necessidades específicas não atendidas pelas abordagens disponíveis.

Quando `GetTempFileName()` é chamado, um arquivo de 0 bytes é criado no disco e o caminho completo para esse arquivo é retornado. É importante lembrar de excluir o arquivo temporário após o uso, caso contrário, ele permanecerá no disco.

## Ver Também:

- [Documentação Oficial da Microsoft sobre a Classe Path](https://docs.microsoft.com/pt-br/dotnet/api/system.io.path?view=net-5.0)
- [Conceitos básicos de E/S de arquivo](https://docs.microsoft.com/pt-br/dotnet/standard/io/)
- [Leitura e gravação para arquivos em C#](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/file-system/how-to-read-from-and-write-to-a-newly-created-data-file)