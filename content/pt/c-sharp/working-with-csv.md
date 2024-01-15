---
title:                "Trabalhando com csv"
html_title:           "C#: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que utilizar CSV

Se você é um programador C# ou está apenas começando a aprender essa linguagem de programação, é provável que já tenha ouvido falar sobre arquivos CSV. Mas, afinal, por que tantas pessoas trabalham com eles?

Bem, a resposta é simples: CSV, ou "Comma Separated Values", é um formato de arquivo simples e amplamente utilizado para armazenar, organizar e compartilhar dados. É especialmente útil para armazenar dados tabulares, como planilhas ou tabelas, e pode ser facilmente importado e exportado por muitos softwares e bancos de dados.

## Como utilizar CSV em C#

Agora que você conhece a utilidade do formato CSV, vamos dar uma olhada em como é simples e fácil trabalhar com ele em C#.

Para começar, é necessário importar o namespace `System.IO`, que contém classes para operações de entrada e saída de dados, incluindo a classe `StreamReader` que nos permitirá ler um arquivo CSV:

```C#
using System.IO;
```

Em seguida, podemos usar o método `ReadLine()` da classe `StreamReader` para ler cada linha do arquivo CSV e armazená-la em uma variável. Para isso, precisaremos abrir e criar uma instância da classe `StreamReader` com o caminho do arquivo CSV como parâmetro:

```C#
StreamReader reader = new StreamReader(@"C:\Users\Usuario\arquivo.csv");
string linha;
while((linha = reader.ReadLine()) != null)
{
    // aqui você pode fazer o que quiser com a linha do arquivo
}
```

Com a linha do arquivo armazenada em uma variável, podemos realizar diferentes tarefas, como separar os valores em colunas, processar os dados e salvá-los em uma estrutura de dados ou imprimir na tela.

Por exemplo, se quisermos armazenar cada valor em uma lista de strings, podemos usar o método `Split()` para separar os valores com base no caractere de separação, que por padrão é a vírgula (`,`) em arquivos CSV:

```C#
string[] valores = linha.Split(',');
```

E para imprimir esses valores na tela, podemos usar um loop `foreach`:

```C#
foreach(string valor in valores)
{
    Console.WriteLine(valor);
}
```

Por fim, para garantir que o arquivo seja fechado corretamente e liberar os recursos utilizados, é importante utilizar o método `Close()` da classe `StreamReader`:

```C#
reader.Close();
```

## Aprofundando-se em CSV

Agora que você já sabe como ler um arquivo CSV em C#, é importante conhecer alguns detalhes importantes sobre o formato.

Embora a maioria dos arquivos CSV sejam separados por vírgulas, as versões mais recentes também permitem outros caracteres de separação, como ponto e vírgula (`;`). Além disso, é possível que os dados estejam delimitados por aspas (`"`), principalmente quando os valores contêm o caractere de separação.

Também é importante ter em mente que arquivos CSV podem não seguir um padrão rígido, podendo conter diferentes tipos de delimitadores, formatos de data e hora, etc. Por isso, é sempre bom testar o seu código com diferentes arquivos para garantir que tudo esteja funcionando corretamente.

A documentação oficial da linguagem C# também possui uma classe `CsvReader` que facilita o trabalho com arquivos CSV e pode ser uma ótima opção caso você precise lidar com arquivos mais complexos ou queiram uma solução pronta.

## Veja também

- [Documentação oficial do C#](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- [Referência da classe CsvReader](https://docs.microsoft.com/pt-br/dotnet/api/microsoft.visualbasic.fileio.textfieldparser?view=netframework-4.8)
- [Tutorial: Como trabalhar com arquivos CSV em C#](https://www.codeproject.com/Articles/415732/Reading-and-Writing-CSV-Files-in-Csharp)