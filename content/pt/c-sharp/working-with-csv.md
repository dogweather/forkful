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

## O que é CSV e por que os programadores o usam?

CSV é um formato de arquivo utilizado para armazenar dados de maneira simples e eficiente. Ele consiste em uma lista de valores separados por vírgulas, onde cada linha representa uma entrada de dados. Programadores muitas vezes usam CSV para importar ou exportar dados de um programa para outro, ou para armazenar dados estruturados em um formato legível por humanos.

## Como fazer:

Para trabalhar com CSV em C#, você pode usar a biblioteca padrão "System.IO", que fornece a classe "StreamReader" para ler arquivos e a classe "StreamWriter" para escrever em arquivos. Primeiro, é preciso abrir o arquivo CSV usando a classe "StreamReader", informando o caminho do arquivo como parâmetro. Em seguida, é possível usar a função "ReadLine()" para ler cada linha do arquivo. Para separar os valores da linha, basta usar o método "Split()" e passar como argumento o delimitador desejado, geralmente uma vírgula. Depois disso, é possível manipular os dados como desejar. Abaixo, segue um exemplo simples de leitura e escrita em um arquivo CSV:

```
using System.IO;

// Lendo arquivo CSV
StreamReader reader = new StreamReader("arquivo.csv");

// Escrevendo em arquivo CSV
StreamWriter writer = new StreamWriter("arquivo.csv");

string line = "";

// Lendo cada linha do arquivo
while((line = reader.ReadLine()) != null)
{
    // Separando valores da linha
    string[] values = line.Split(',');

    // Fazendo alterações nos valores
    values[1] = values[1] * 2;

    // Escrevendo alterações no arquivo
    writer.WriteLine(string.Join(",", values));
}

// Fechando arquivos
reader.Close();
writer.Close();
```

## Mergulho profundo:

O formato CSV é amplamente utilizado desde a década de 1970 para facilitar a troca de dados entre diferentes sistemas. Ele é um formato simples e leve, o que o torna popular entre os programadores. Além disso, por ser um arquivo de texto, é facilmente editável por qualquer pessoa com um editor de texto básico. No entanto, existem outros formatos de arquivos de dados mais avançados, como JSON e XML, que podem ser mais adequados para determinadas situações.

## Veja também:

- [Documentação oficial do C#](https://docs.microsoft.com/pt-br/dotnet/csharp/)