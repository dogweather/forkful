---
title:                "Escrevendo um arquivo de texto"
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever um arquivo de texto é o processo de salvar dados em um arquivo no disco com formato legível. Programadores fazem isso para log de dados, configurações, ou para salvar informações que podem ser lidas por outros programas ou pessoas.

## Como Fazer:

```C#
using System;
using System.IO;

class Programa {
    static void Main() {
        string caminho = "exemplo.txt";
        string conteudo = "Olá, Mundo! Esse é um exemplo de escrita em arquivo.";

        // Usando File.WriteAllText
        File.WriteAllText(caminho, conteudo);
        
        // Usando StreamWriter
        using (StreamWriter sw = new StreamWriter(caminho)) {
            sw.WriteLine(conteudo);
        }

        // Confirmação de escrita
        Console.WriteLine(File.ReadAllText(caminho));
    }
}
```

Saída:
```
Olá, Mundo! Esse é um exemplo de escrita em arquivo.
```

## Visão Detalhada

Antigamente, a escrita em arquivos era feita usando-se linguagens de baixo nível, lidando diretamente com o sistema operacional. A simplificação veio com bibliotecas e frameworks, como o .NET de C#. Alternativas ao escrita de arquivos incluem bancos de dados e armazenamento em nuvem, mas arquivos texto são simples e amplamente usados. Em C#, `StreamWriter` e `File.WriteAllText` são maneiras comuns de escrever em arquivos, onde `StreamWriter` é mais apropriado para grandes quantidades de dados ou para anexar a arquivos existentes.

## Veja Também

- Documentação oficial do `StreamWriter`: [StreamWriter Classe](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamwriter?view=net-6.0)
- Documentação oficial do `File`: [File Classe](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file?view=net-6.0)
- Tutorial sobre manipulação de arquivos em C#: [Tutorial de Arquivos em C#](https://docs.microsoft.com/pt-br/dotnet/standard/io/how-to-write-text-to-a-file)
