---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um arquivo de texto em C#

## O que e porque?

Ler um arquivo de texto implica acessar e interpretar os dados contidos em um arquivo de texto. Programadores fazem isso para manipular, analisar ou reformatar informações armazenadas em formato textual.

## Como fazer:

A leitura de um arquivo de texto em C# pode ser feita de várias maneiras. Vamos olhar para o método mais simples usando a classe 'StreamReader'.

```C#
using System;
using System.IO;

class LeituraArquivoTexto {
    static void Main() {
        using (StreamReader leitor = new StreamReader("arquivo.txt")) {
            string linha;
            
            while((linha = leitor.ReadLine()) != null) {
                Console.WriteLine(linha);
            }
        }
    }
}
```
Esse código simplesmente abre o arquivo "arquivo.txt", lê todas as linhas uma a uma e imprime no console.

## Aprofundando-se

A classe 'StreamReader' faz parte do namespace 'System.IO' e tem sido usada para ler arquivos desde as primeiras versões do .NET Framework, tornando-a uma das classes mais antigas dentro do C#.

Você tem várias alternativas para ler um arquivo em C#. 'File' e 'Fileinfo' são duas classes que você pode usar para ler um arquivo de texto. Usar a classe 'File' é mais fácil em termos de menos linhas de código, porém menos versátil.

Vale ressaltar que o método ReadLine da classe 'StreamReader' lê uma linha de texto até que uma nova linha seja encontrada ou até o final do arquivo, retornando 'null' neste último caso.

## Veja também:

1. Documentação oficial 'StreamReader': [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
2. Classe 'File': [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)
3. Classe 'FileInfo': [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.fileinfo?view=net-5.0)
4. Método 'ReadLine': [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader.readline?view=net-5.0)